;;;; src/swank/tools.lisp - MCP tools for Swank integration

(in-package :cl-tron-mcp/swank)

;;; These tools enable MCP clients (like Opencode) to interact with
;;; a running SBCL instance via Swank.

;;; ============================================================
;;; Connection Tools
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
;;; Evaluation Tools
;;; ============================================================

(defun mcp-swank-eval (code &key (package "CL-USER"))
  "Evaluate Lisp code via Swank.
   
   This is the primary tool for executing Lisp code in the
   running SBCL instance connected via Swank.
   
   Example:
     code: \"(+ 10 20)\"
     package: \"CL-USER\"
   
   Returns: Evaluation result"
  (swank-eval code :package package))

(defun mcp-swank-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via Swank.
   
   Returns: Compilation result with success/failure"
  (swank-compile-string code :package package :filename filename))

;;; ============================================================
;;; Debugging Tools
;;; ============================================================

(defun mcp-swank-threads ()
  "List all threads in the Swank-connected SBCL."
  (swank-threads))

(defun mcp-swank-abort (&optional (thread-id nil))
  "Abort a specific thread, or the current thread if not specified."
  (swank-abort-thread thread-id))

(defun mcp-swank-interrupt ()
  "Interrupt the current thread's execution."
  (swank-interrupt))

(defun mcp-swank-backtrace (&optional (thread :repl-thread))
  "Get the current backtrace."
  (swank-backtrace thread))

(defun mcp-swank-frame-locals (frame &optional (thread :repl-thread))
  "Get local variables for a specific frame."
  (swank-frame-locals frame thread))

;;; ============================================================
;;; Inspector Tools
;;; ============================================================

(defun mcp-swank-inspect (expression)
  "Inspect an object via Swank.
   
   Example:
     expression: \"*package*\" or \"(make-hash-table)\"
   
   Returns: Swank inspection result"
  (let ((obj (read-from-string expression)))
    (swank-inspect obj)))

(defun mcp-swank-describe (expression)
  "Describe an object via Swank.
   
   Example:
     expression: \"car\" or \"list\"
   
   Returns: Swank description"
  (let ((obj (read-from-string expression)))
    (swank-describe obj)))

;;; ============================================================
;;; REPL Tools
;;; ============================================================

(defun mcp-swank-repl (&optional (package "CL-USER"))
  "Switch to a REPL for a specific package."
  (swank-switch-to-repl package))

(defun mcp-swank-clear ()
  "Clear the REPL buffer."
  (swank-clear-repl))

;;; ============================================================
;;; Autodoc Tools
;;; ============================================================

(defun mcp-swank-autodoc (symbol &optional (thread :repl-thread))
  "Get documentation for a symbol.
   
   Example:
     symbol: \"(car list)\" or \"mapcar\"
   
   Returns: Autodoc information"
  (swank-autodoc symbol thread))

;;; ============================================================
;;; Completion Tools
;;; ============================================================

(defun mcp-swank-completions (symbol &optional (package "CL-USER"))
  "Get symbol completions via Swank.
   
   Example:
     symbol: \"mak\"  ; Returns (make-array make-hash-table ...)
   
   Returns: List of completions"
  (swank-completions symbol package))

;;; ============================================================
;;; Utility
;;; ============================================================

(defun swank-help ()
  "Get help on available Swank tools."
  (list :tools (list
               (list :name "swank_connect" :description "Connect to Swank server")
               (list :name "swank_disconnect" :description "Disconnect from Swank server")
               (list :name "swank_eval" :description "Evaluate Lisp code")
               (list :name "swank_compile" :description "Compile Lisp code")
               (list :name "swank_threads" :description "List threads")
               (list :name "swank_abort" :description "Abort a thread")
               (list :name "swank_interrupt" :description "Interrupt evaluation")
               (list :name "swank_backtrace" :description "Get backtrace")
               (list :name "swank_inspect" :description "Inspect an object")
               (list :name "swank_describe" :description "Describe an object")
               (list :name "swank_repl" :description "Switch to REPL")
               (list :name "swank_autodoc" :description "Get symbol documentation")
               (list :name "swank_completions" :description "Get symbol completions"))
        :note "Before using Swank tools, connect to a running SBCL with Swank loaded"))
