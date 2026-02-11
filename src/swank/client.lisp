;;;; src/swank/client.lisp - Swank client for CL-TRON-MCP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t))

(in-package :cl-tron-mcp/swank)

;;; This module provides Swank connection tools for CL-TRON-MCP.
;;; It enables MCP clients to connect to a running SBCL with Swank loaded.

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defvar *swank-server* nil
  "Swank server connection")

(defvar *swank-io* nil
  "Swank I/O streams")

(defvar *swank-connected* nil
  "Connection status")

(defun connect-swank (&key (host "127.0.0.1") (port 4005) (timeout 10))
  "Connect to a Swank server.
   
   Usage:
     (cl-tron-mcp/swank:connect-swank :port 4005)
   
   Returns: Connection info or error"
  (when *swank-connected*
    (return-from connect-swank
      (list :error t :message "Already connected to Swank server")))
  
  (handler-case
      (let ((socket (usocket:socket-connect host port :timeout (* timeout 1000))))
        (setq *swank-server* socket
              *swank-io* (usocket:socket-stream socket)
              *swank-connected* t)
        (list :success t
              :host host
              :port port
              :message "Connected to Swank server"))
    (error (e)
      (list :error t
            :message (format nil "Failed to connect to Swank: ~a" e)))))

(defun disconnect-swank ()
  "Disconnect from Swank server."
  (when *swank-server*
    (ignore-errors (usocket:socket-close *swank-server*))
    (setq *swank-server* nil
          *swank-io* nil
          *swank-connected* nil))
  (list :success t :message "Disconnected from Swank server"))

(defun swank-connected-p ()
  "Check if connected to Swank."
  (list :connected *swank-connected*))

;;; ============================================================
;;; MCP Wrapper Functions
;;; ============================================================

(defun swank-connect (&key (host "127.0.0.1") (port 4005))
  "Connect to a running SBCL with Swank loaded.
   
   On the SBCL side, you should have:
     (ql:quickload :swank)
     (swank:create-server :port 4005)
   
   Returns: Connection status"
  (let ((result (connect-swank :host host :port port :timeout 30)))
    (if (getf result :error)
        result
        (list :success t
              :message (format nil "Connected to Swank at ~a:~a" host port)
              :host host
              :port port))))

(defun swank-disconnect ()
  "Disconnect from the Swank server."
  (disconnect-swank))

(defun swank-status ()
  "Get the current Swank connection status."
  (list :connected *swank-connected*
        :has-connection (not (null *swank-server*))))

;;; ============================================================
;;; TCP Message Exchange
;;; ============================================================

(defun send-swank-message (message)
  "Send a message to Swank and get response."
  (unless *swank-connected*
    (return-from send-swank-message
      (list :error t :message "Not connected to Swank server")))
  
  (handler-case
      (progn
        (format *swank-io* "~a~%" message)
        (force-output *swank-io*)
        (let ((response (read *swank-io* nil :eof)))
          (if (eq response :eof)
              (list :error t :message "Connection closed by server")
              response)))
    (error (e)
      (list :error t :message (princ-to-string e)))))

;;; ============================================================
;;; Evaluation
;;; ============================================================

(defun swank-eval-internal (code &key (package "CL-USER"))
  "Evaluate Lisp code via the Swank connection."
  (unless *swank-connected*
    (return-from swank-eval-internal
      (list :error t :message "Not connected to Swank server")))
  (let ((request `(:eval ,code :package ,package)))
    (send-swank-message request)))

(defun mcp-swank-eval (code &key (package "CL-USER"))
  "Evaluate Lisp code via Swank.
   
   Example:
     code: \"(+ 10 20)\"
     package: \"CL-USER\"
   
   Returns: Evaluation result"
  (swank-eval-internal code :package package))

(defun swank-compile-internal (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via the Swank connection."
  (unless *swank-connected*
    (return-from swank-compile-internal
      (list :error t :message "Not connected to Swank server")))
  (let ((request `(:compile ,code :package ,package :filename ,filename)))
    (send-swank-message request)))

(defun mcp-swank-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via Swank."
  (swank-compile-internal code :package package :filename filename))

;;; ============================================================
;;; Thread Operations
;;; ============================================================

(defun mcp-swank-threads ()
  "List all threads in the Swank-connected SBCL."
  (send-swank-message '(:list-threads)))

(defun mcp-swank-abort (&optional (thread-id nil))
  "Abort a thread."
  (send-swank-message `(:abort-thread ,thread-id)))

(defun mcp-swank-interrupt ()
  "Interrupt the current thread."
  (send-swank-message '(:interrupt)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(defun mcp-swank-backtrace (&optional (thread :repl-thread))
  "Get the current backtrace."
  (send-swank-message `(:backtrace ,thread)))

(defun mcp-swank-frame-locals (frame &optional (thread :repl-thread))
  "Get local variables for a frame."
  (send-swank-message `(:frame-locals ,frame ,thread)))

;;; ============================================================
;;; Inspector
;;; ============================================================

(defun mcp-swank-inspect (expression)
  "Inspect an object via Swank.
   
   Example:
     expression: \"*package*\" or \"(make-hash-table)\""
  (let ((obj (read-from-string expression)))
    (send-swank-message `(:inspect ,obj))))

(defun mcp-swank-describe (expression)
  "Describe an object via Swank.
   
   Example:
     expression: \"car\" or \"list\""
  (let ((obj (read-from-string expression)))
    (send-swank-message `(:describe ,obj))))

;;; ============================================================
;;; Documentation
;;; ============================================================

(defun mcp-swank-autodoc (symbol)
  "Get documentation for a symbol.
   
   Example:
     symbol: \"mapcar\" or \"(car list)\""
  (send-swank-message `(:autodoc ,symbol)))

(defun mcp-swank-completions (symbol &optional (package "CL-USER"))
  "Get symbol completions via Swank.
   
   Example:
     symbol: \"mak\" returns (make-array make-hash-table ...)"
  (send-swank-message `(:completions ,symbol ,package)))

;;; ============================================================
;;; Help
;;; ============================================================

(defun swank-help ()
  "Get help on available Swank tools."
  (list :tools (list
               (list :name "swank_connect" :description "Connect to Swank server")
               (list :name "swank_disconnect" :description "Disconnect from Swank server")
               (list :name "swank_eval" :description "Evaluate Lisp code")
               (list :name "swank_compile" :description "Compile Lisp code")
               (list :name "swank_threads" :description "List threads")
               (list :name "swank_abort" :description "Abort thread")
               (list :name "swank_interrupt" :description "Interrupt evaluation")
               (list :name "swank_backtrace" :description "Get backtrace")
               (list :name "swank_inspect" :description "Inspect object")
               (list :name "swank_describe" :description "Describe object")
               (list :name "swank_autodoc" :description "Get documentation")
               (list :name "swank_completions" :description "Get completions"))
        :note "Connect to Swank first: (swank-connect :port 4005)"))
