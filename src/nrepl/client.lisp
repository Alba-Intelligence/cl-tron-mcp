;;;; src/nrepl/client.lisp - nrepl client for CL-TRON-MCP
;;;;
;;;; nrepl uses bencode (not JSON) for message serialization.
;;;; Messages are dictionaries with string keys.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :bencode :silent t))

(in-package :cl-tron-mcp/nrepl)

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *nrepl-server* nil
  "nrepl server socket")

(defvar *nrepl-stream* nil
  "nrepl I/O stream")

(defvar *nrepl-connected* nil
  "Connection status")

(defvar *nrepl-session* nil
  "Current nrepl session ID")

(defvar *nrepl-message-id* 0
  "Message ID counter")

;;; ============================================================
;;; Bencode Helper Functions
;;; ============================================================
;;;
;;; We use the quicklisp bencode library for serialization.

(defun make-nrepl-message (&rest key-values)
  "Create a hash-table suitable for bencode encoding.
KEY-VALUES should be alternating keys and values."
  (let ((msg (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (setf (gethash (string key) msg)
                   (if (integerp value) value (princ-to-string value))))
    msg))

(defun hash-table-to-plist (ht)
  "Convert a hash-table to a plist for easier access."
  (let ((result nil))
    (maphash (lambda (k v)
               (push (intern (string-upcase k) :keyword) result)
               (push v result))
             ht)
    (nreverse result)))

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun nrepl-connect (&key (host "127.0.0.1") (port 8675) (timeout 10))
  "Connect to an nrepl server (cl-nrepl, Clojure nrepl, etc.)

   Usage:
     (cl-tron-mcp/nrepl:nrepl-connect :host \"127.0.0.1\" :port 8675)

   On the SBCL side with cl-nrepl:
     (pushnew #p\"/path/to/cl-nrepl/\" asdf:*central-registry* :test #'equal)
     (ql:quickload :nrepl)
     (nrepl:start-server :port 8675)

   Returns: Connection info or error"
  (when *nrepl-connected*
    (return-from nrepl-connect
      (list :error t :message "Already connected to nrepl server")))
  
  (handler-case
      (let ((socket (usocket:socket-connect host port :timeout timeout
                                            :element-type '(unsigned-byte 8))))
        (setq *nrepl-server* socket
              *nrepl-stream* (usocket:socket-stream socket)
              *nrepl-connected* t)
        ;; Create a new session
        (let ((response (nrepl-send-raw "op" "clone")))
          (let ((session (getf response :new-session)))
            (when session
              (setq *nrepl-session* session))))
        (list :success t
              :host host
              :port port
              :session *nrepl-session*
              :message "Connected to nrepl server"))
    (error (e)
      (list :error t
            :message (format nil "Failed to connect to nrepl: ~a" e)))))

(defun nrepl-disconnect ()
  "Disconnect from nrepl server."
  (when *nrepl-session*
    (ignore-errors (nrepl-send-raw "op" "close" "session" *nrepl-session*)))
  (when *nrepl-stream*
    (ignore-errors (close *nrepl-stream*)))
  (when *nrepl-server*
    (ignore-errors (usocket:socket-close *nrepl-server*)))
  (setq *nrepl-server* nil
        *nrepl-stream* nil
        *nrepl-session* nil
        *nrepl-connected* nil)
  (list :success t :message "Disconnected from nrepl server"))

(defun nrepl-connected-p ()
  "Check if connected to nrepl."
  (and *nrepl-connected* *nrepl-stream*))

(defun nrepl-status ()
  "Get the current nrepl connection status."
  (list :connected *nrepl-connected*
        :session *nrepl-session*
        :has-connection (not (null *nrepl-stream*))))

;;; ============================================================
;;; Message Exchange
;;; ============================================================

(defun nrepl-next-id ()
  "Generate next message ID."
  (incf *nrepl-message-id*))

(defun read-nrepl-message ()
  "Read a bencode message from nrepl."
  (handler-case
      (let ((result (bencode:decode *nrepl-stream*)))
        (hash-table-to-plist result))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun write-nrepl-message (msg)
  "Write a bencode message to nrepl.
MSG should be a hash-table with string keys."
  (bencode:encode msg *nrepl-stream*)
  (force-output *nrepl-stream*))

(defun nrepl-send-raw (&rest args)
  "Send a message to nrepl and get response.
ARGS are alternating key-value pairs."
  (unless *nrepl-connected*
    (return-from nrepl-send-raw
      (list :error t :message "Not connected to nrepl server")))
  
  (let* ((msg-id (nrepl-next-id))
         (msg (make-nrepl-message "id" msg-id)))
    (loop for (key value) on args by #'cddr
          do (setf (gethash (string key) msg)
                   (if (integerp value) value (princ-to-string value))))
    (write-nrepl-message msg)
    (read-nrepl-message)))

(defun nrepl-send (&rest args)
  "Send a message with session to nrepl."
  (unless *nrepl-connected*
    (return-from nrepl-send
      (list :error t :message "Not connected to nrepl server")))
  
  (apply #'nrepl-send-raw "session" *nrepl-session* args))

;;; ============================================================
;;; Evaluation
;;; ============================================================

(defun nrepl-eval (&key code (package "CL-USER"))
  "Evaluate Lisp code via nrepl.

   Example:
     code: \"(+ 10 20)\"
     package: \"CL-USER\"

   Returns: Evaluation result"
  (unless code
    (return-from nrepl-eval
      (list :error t :message "code is required")))
  (unless *nrepl-connected*
    (return-from nrepl-eval
      (list :error t :message "Not connected to nrepl server")))
  (let ((response (nrepl-send "op" "eval" "code" code "ns" package)))
    ;; Extract value from response
    (when (getf response :value)
      (setf (getf response :result) (getf response :value)))
    response))

(defun nrepl-compile (&key code (package "CL-USER") (filename "repl"))
  "Compile Lisp code via nrepl."
  (unless code
    (return-from nrepl-compile
      (list :error t :message "code is required")))
  (unless *nrepl-connected*
    (return-from nrepl-compile
      (list :error t :message "Not connected to nrepl server")))
  (nrepl-send "op" "compile" "code" code "ns" package "filename" filename))

;;; ============================================================
;;; Session Operations
;;; ============================================================

(defun nrepl-sessions ()
  "List all nrepl sessions."
  (nrepl-send "op" "sessions"))

(defun nrepl-close-session (&optional session)
  "Close an nrepl session."
  (declare (ignore session))
  (nrepl-send "op" "close"))

;;; ============================================================
;;; Thread Operations
;;; ============================================================

(defun nrepl-threads ()
  "List all threads in the nrepl-connected SBCL."
  (nrepl-send "op" "list-threads"))

(defun nrepl-interrupt ()
  "Interrupt the current evaluation."
  (nrepl-send "op" "interrupt"))

;;; ============================================================
;;; Debugging
;;; ============================================================

(defun nrepl-backtrace (&optional thread)
  "Get the current backtrace."
  (declare (ignore thread))
  (nrepl-send "op" "backtrace"))

;;; ============================================================
;;; Inspector
;;; ============================================================

(defun nrepl-inspect (&key expression)
  "Inspect an object via nrepl."
  (unless expression
    (return-from nrepl-inspect (list :error t :message "expression is required")))
  (nrepl-send "op" "inspect" "expression" expression))

(defun nrepl-describe (&key symbol)
  "Describe a symbol via nrepl."
  (unless symbol
    (return-from nrepl-describe (list :error t :message "symbol is required")))
  (nrepl-send "op" "describe-symbol" "symbol" symbol))

;;; ============================================================
;;; Documentation
;;; ============================================================

(defun nrepl-doc (&key symbol)
  "Get documentation for a symbol."
  (unless symbol
    (return-from nrepl-doc (list :error t :message "symbol is required")))
  (nrepl-send "op" "apropos" "symbol" symbol))

(defun nrepl-completions (&key prefix (package "CL-USER"))
  "Get symbol completions via nrepl.

   Example:
     prefix: \"mak\" returns (make-array make-hash-table ...)"
  (unless prefix
    (return-from nrepl-completions (list :error t :message "prefix is required")))
  (nrepl-send "op" "complete" "prefix" prefix "ns" package))

;;; ============================================================
;;; Help
;;; ============================================================

(defun nrepl-help ()
  "Get help on available nrepl tools."
  (list :tools (list
                (list :name "nrepl_connect" :description "Connect to nrepl server")
                (list :name "nrepl_disconnect" :description "Disconnect from nrepl server")
                (list :name "nrepl_status" :description "Check connection status")
                (list :name "nrepl_eval" :description "Evaluate Lisp code")
                (list :name "nrepl_compile" :description "Compile Lisp code")
                (list :name "nrepl_sessions" :description "List nrepl sessions")
                (list :name "nrepl_close_session" :description "Close a session")
                (list :name "nrepl_threads" :description "List threads")
                (list :name "nrepl_interrupt" :description "Interrupt evaluation")
                (list :name "nrepl_backtrace" :description "Get backtrace")
                (list :name "nrepl_inspect" :description "Inspect object")
                (list :name "nrepl_describe" :description "Describe symbol")
                (list :name "nrepl_doc" :description "Get documentation")
                (list :name "nrepl_completions" :description "Get completions"))
        :note "Connect to nrepl first: (nrepl-connect :port 7888)"))
