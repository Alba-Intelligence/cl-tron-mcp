;;;; src/nrepl/client.lisp - nrepl client for CL-TRON-MCP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :jonathan :silent t))

(in-package :cl-tron-mcp/nrepl)

;;; ============================================================
;;; Connection Management
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

(defun nrepl-connect (&key (host "127.0.0.1") (port 7888) (timeout 10))
  "Connect to an nrepl server (Sly, CIDER, etc.)

   Usage:
     (cl-tron-mcp/nrepl:nrepl-connect :host \"127.0.0.1\" :port 7888)

   On the SBCL side with Sly or CIDER:
     (ql:quickload :sly)  ; or :cider
     (sly:nrepl-start :port 7888)

   Returns: Connection info or error"
  (when *nrepl-connected*
    (return-from nrepl-connect
      (list :error t :message "Already connected to nrepl server")))

  (handler-case
      (let ((socket (usocket:socket-connect host port :timeout (* timeout 1000))))
        (setq *nrepl-server* socket
              *nrepl-stream* (usocket:socket-stream socket)
              *nrepl-connected* t)
        ;; Read the greeting
        (read-nrepl-message)
        ;; Create a new session
        (let ((response (nrepl-send-raw :op "clone" :id 1)))
          (when (getf response :new-session)
            (setq *nrepl-session* (getf response :new-session))))
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
    (ignore-errors (nrepl-send-raw :op "close" :session *nrepl-session*)))
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
  (list :connected *nrepl-connected*))

(defun nrepl-status ()
  "Get the current nrepl connection status."
  (list :connected *nrepl-connected*
        :session *nrepl-session*
        :has-connection (not (null *nrepl-stream*))))

;;; ============================================================
;;; JSON Message Exchange
;;; ============================================================

(defun nrepl-next-id ()
  "Generate next message ID."
  (incf *nrepl-message-id*))

(defun read-nrepl-message ()
  "Read a JSON message from nrepl."
  (handler-case
      (let ((line (read-line *nrepl-stream* nil :eof)))
        (if (eq line :eof)
            (list :error t :message "Connection closed by server")
            (jonathan:parse line)))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun write-nrepl-message (alist)
  "Write a JSON message to nrepl."
  (let ((json (jonathan:to-json alist)))
    (format *nrepl-stream* "~a~%" json)
    (force-output *nrepl-stream*)))

(defun nrepl-send-raw (&rest args)
  "Send a message to nrepl and get response."
  (unless *nrepl-connected*
    (return-from nrepl-send-raw
      (list :error t :message "Not connected to nrepl server")))

  (let* ((msg-id (nrepl-next-id))
         (msg (append (list :id msg-id) args)))
    (write-nrepl-message msg)
    (read-nrepl-message)))

(defun nrepl-send (&rest args)
  "Send a message with session to nrepl."
  (unless *nrepl-connected*
    (return-from nrepl-send
      (list :error t :message "Not connected to nrepl server")))

   (let* ((msg-id (nrepl-next-id))
          (msg (append (list :id msg-id :session *nrepl-session*) args)))
    (write-nrepl-message msg)
    (read-nrepl-message)))

;;; ============================================================
;;; Evaluation
;;; ============================================================

(defun nrepl-eval (code &key (package "CL-USER") (session nil))
  "Evaluate Lisp code via nrepl.

   Example:
     code: \"(+ 10 20)\"
     package: \"CL-USER\"

   Returns: Evaluation result"
  (unless *nrepl-connected*
    (return-from nrepl-eval
      (list :error t :message "Not connected to nrepl server")))
  (let* ((response (nrepl-send :op "eval"
                               :code code
                               :ns package)))
    ;; Extract value from response
    (when (getf response :|value|)
      (setf (getf response :result) (getf response :|value|)))
    response))

(defun nrepl-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via nrepl."
  (unless *nrepl-connected*
    (return-from nrepl-compile
      (list :error t :message "Not connected to nrepl server")))
  (nrepl-send :op "compile"
              :code code
              :ns package
              :filename filename))

;;; ============================================================
;;; Session Operations
;;; ============================================================

(defun nrepl-sessions ()
  "List all nrepl sessions."
  (nrepl-send :op "sessions"))

(defun nrepl-close-session (&optional (session nil))
  "Close an nrepl session."
  (nrepl-send :op "close"))

;;; ============================================================
;;; Thread Operations
;;; ============================================================

(defun nrepl-threads ()
  "List all threads in the nrepl-connected SBCL."
  (nrepl-send :op "list-threads"))

(defun nrepl-interrupt ()
  "Interrupt the current evaluation."
  (nrepl-send :op "interrupt"))

;;; ============================================================
;;; Debugging
;;; ============================================================

(defun nrepl-backtrace (&optional (thread nil))
  "Get the current backtrace."
  (nrepl-send :op "backtrace"))

;;; ============================================================
;;; Inspector
;;; ============================================================

(defun nrepl-inspect (expression)
  "Inspect an object via nrepl."
  (nrepl-send :op "inspect"
              :expression expression))

(defun nrepl-describe (symbol)
  "Describe a symbol via nrepl."
  (nrepl-send :op "describe-symbol"
              :symbol symbol))

;;; ============================================================
;;; Documentation
;;; ============================================================

(defun nrepl-doc (symbol)
  "Get documentation for a symbol."
  (nrepl-send :op "apropos"
              :symbol symbol))

(defun nrepl-completions (prefix)
  "Get symbol completions via nrepl.

   Example:
     prefix: \"mak\" returns (make-array make-hash-table ...)"
  (nrepl-send :op "complete"
              :prefix prefix))

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
