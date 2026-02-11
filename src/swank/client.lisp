;;;; src/swank/client.lisp - Swank client for CL-TRON-MCP

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

;;; Quickload usocket for TCP connections
#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t))

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
;;; Simple Evaluation (via custom TCP protocol)
;;; ============================================================

(defun swank-eval (code &key (package "CL-USER"))
  "Evaluate Lisp code via the Swank connection.
   
   This sends an S-expression to the Swank server for evaluation.
   
   Usage:
     (cl-tron-mcp/swank:swank-eval \"(+ 10 20)\")
   
   Returns: Evaluation result"
  (unless *swank-connected*
    (return-from swank-eval
      (list :error t :message "Not connected to Swank server")))
  
  (let ((request `(:eval ,code :package ,package)))
    (send-swank-message request)))

(defun swank-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via the Swank connection."
  (unless *swank-connected*
    (return-from swank-compile
      (list :error t :message "Not connected to Swank server")))
  
  (let ((request `(:compile ,code :package ,package :filename ,filename)))
    (send-swank-message request)))

;;; ============================================================
;;; Thread Operations
;;; ============================================================

(defun swank-threads ()
  "List threads via Swank."
  (unless *swank-connected*
    (return-from swank-threads
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message '(:list-threads)))

(defun swank-abort-thread (&optional (thread-id nil))
  "Abort a thread."
  (unless *swank-connected*
    (return-from swank-abort-thread
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:abort-thread ,thread-id)))

(defun swank-interrupt ()
  "Interrupt the current thread."
  (unless *swank-connected*
    (return-from swank-interrupt
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message '(:interrupt)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(defun swank-backtrace (&optional (thread :repl-thread))
  "Get backtrace."
  (unless *swank-connected*
    (return-from swank-backtrace
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:backtrace ,thread)))

(defun swank-frame-locals (frame &optional (thread :repl-thread))
  "Get local variables for a frame."
  (unless *swank-connected*
    (return-from swank-frame-locals
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:frame-locals ,frame ,thread)))

;;; ============================================================
;;; Inspector
;;; ============================================================

(defun swank-inspect (expression)
  "Inspect an expression."
  (unless *swank-connected*
    (return-from swank-inspect
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:inspect ,expression)))

(defun swank-describe (expression)
  "Describe an expression."
  (unless *swank-connected*
    (return-from swank-describe
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:describe ,expression)))

;;; ============================================================
;;; Documentation
;;; ============================================================

(defun swank-autodoc (symbol)
  "Get documentation for a symbol."
  (unless *swank-connected*
    (return-from swank-autodoc
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:autodoc ,symbol)))

(defun swank-completions (symbol &optional (package "CL-USER"))
  "Get symbol completions."
  (unless *swank-connected*
    (return-from swank-completions
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:completions ,symbol ,package)))

;;; ============================================================
;;; REPL
;;; ============================================================

(defun swank-switch-to-repl (&optional (package "CL-USER"))
  "Switch to a REPL for a package."
  (unless *swank-connected*
    (return-from swank-switch-to-repl
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message `(:repl ,package)))

(defun swank-clear-repl ()
  "Clear the REPL."
  (unless *swank-connected*
    (return-from swank-clear-repl
      (list :error t :message "Not connected to Swank server")))
  
  (send-swank-message '(:clear-repl)))

;;; ============================================================
;;; MCP Tool Wrappers
;;; ============================================================

(defun mcp-swank-eval (code &key (package "CL-USER"))
  "Evaluate Lisp code via Swank."
  (swank-eval code :package package))

(defun mcp-swank-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via Swank."
  (swank-compile code :package package :filename filename))

(defun mcp-swank-threads ()
  "List threads."
  (swank-threads))

(defun mcp-swank-abort (&optional (thread-id nil))
  "Abort a thread."
  (swank-abort-thread thread-id))

(defun mcp-swank-interrupt ()
  "Interrupt thread."
  (swank-interrupt))

(defun mcp-swank-backtrace ()
  "Get backtrace."
  (swank-backtrace))

(defun mcp-swank-frame-locals (frame)
  "Get frame locals."
  (swank-frame-locals frame))

(defun mcp-swank-inspect (expression)
  "Inspect expression."
  (swank-inspect expression))

(defun mcp-swank-describe (expression)
  "Describe expression."
  (swank-describe expression))

(defun mcp-swank-autodoc (symbol)
  "Get autodoc."
  (swank-autodoc symbol))

(defun mcp-swank-completions (symbol &optional (package "CL-USER"))
  "Get completions."
  (swank-completions symbol package))

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
