;;;; src/unified/client.lisp - Unified REPL interface for CL-TRON-MCP

(in-package :cl-tron-mcp/unified)

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *repl-connected* nil
  "Connection status")

(defvar *repl-type* nil
  "Current REPL type (:swank or :nrepl)")

(defvar *repl-port* nil
  "Connected port")

(defvar *repl-host* nil
  "Connected host")

;;; ============================================================
;;; Unified Connection
;;; ============================================================

(defun repl-connect (&key (type :auto) (host "127.0.0.1") (port 4005))
  "Connect to a Lisp REPL (Swank or nrepl).

   Usage:
     ;; Auto-detect (tries Swank first, then nrepl)
     (repl-connect :port 4005)

     ;; Explicit Swank (Slime, Portacle)
     (repl-connect :type :swank :port 4005)

     ;; Explicit nrepl (Sly, CIDER)
     (repl-connect :type :nrepl :port 7888)

   Returns: Connection status with :type indicating protocol"
  (when *repl-connected*
    (return-from repl-connect
      (list :error t :message "Already connected to a REPL")))

  (let ((actual-type (if (eq type :auto)
                        (auto-detect-repl host port)
                        type)))
    (cond
      ((eq actual-type :swank)
       (let ((result (swank-connect :host host :port port)))
         (when (getf result :success)
           (setq *repl-connected* t
                 *repl-type* :swank
                 *repl-port* port
                 *repl-host* host))
         (list* result (list :type :swank))))

      ((eq actual-type :nrepl)
       (let ((result (nrepl-connect :host host :port port)))
         (when (getf result :success)
           (setq *repl-connected* t
                 *repl-type* :nrepl
                 *repl-port* port
                 *repl-host* host))
         (list* result (list :type :nrepl))))

      (t
       (list :error t :message "Could not detect REPL type")))))

(defun auto-detect-repl (host port)
  "Try to detect REPL type by connecting."
  (handler-case
      (progn
        ;; Try Swank first
        (let ((swank-result (swank-connect :host host :port port :timeout 2)))
          (when (getf swank-result :success)
            (swank-disconnect)
            (return-from auto-detect-repl :swank))))
    (error nil))

  (handler-case
      (progn
        ;; Try nrepl
        (let ((nrepl-result (nrepl-connect :host host :port port :timeout 2)))
          (when (getf nrepl-result :success)
            (nrepl-disconnect)
            (return-from auto-detect-repl :nrepl))))
    (error nil))

  nil)

(defun repl-disconnect ()
  "Disconnect from the current REPL."
  (when *repl-connected*
    (if (eq *repl-type* :swank)
        (swank-disconnect)
        (nrepl-disconnect))
    (setq *repl-connected* nil
          *repl-type* nil))
  (list :success t :message "Disconnected from REPL"))

(defun repl-status ()
  "Get the current REPL connection status."
  (list :connected *repl-connected*
        :type *repl-type*
        :host *repl-host*
        :port *repl-port*))

;;; ============================================================
;;; Unified Evaluation
;;; ============================================================

(defun repl-eval (code &key (package "CL-USER"))
  "Evaluate Lisp code via the connected REPL.

   Example:
     (repl-eval \"(+ 10 20)\")
     (repl-eval \"(defun hello () 'world)\" :package \"MY-PACKAGE\")

   Returns: Evaluation result"
  (unless *repl-connected*
    (return-from repl-eval
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (let ((result (mcp-swank-eval :code code :package package)))
        ;; Normalize response
        (when (getf result :value)
          (setf (getf result :result) (getf result :value)))
        result)
      (let ((result (nrepl-eval :code code :package package)))
        ;; Normalize response
        (when (getf result :|value|)
          (setf (getf result :result) (getf result :|value|)))
        result)))

(defun repl-compile (code &key (package "CL-USER") (filename "repl"))
  "Compile Lisp code via the connected REPL."
  (unless *repl-connected*
    (return-from repl-compile
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-compile :code code :package package :filename filename)
      (nrepl-compile :code code :package package :filename filename)))

;;; ============================================================
;;; Unified Operations
;;; ============================================================

(defun repl-threads ()
  "List all threads in the connected SBCL."
  (unless *repl-connected*
    (return-from repl-threads
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-threads)
       (nrepl-threads)))

(defun repl-abort (&optional (thread nil))
  "Abort a thread or interrupt evaluation."
  (unless *repl-connected*
    (return-from repl-abort
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-abort thread)
       (nrepl-threads)))

(defun repl-backtrace (&optional (thread nil))
  "Get the current backtrace."
  (unless *repl-connected*
    (return-from repl-backtrace
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-backtrace)
       (nrepl-backtrace)))

(defun repl-inspect (expression)
  "Inspect an object via the connected REPL."
  (unless *repl-connected*
    (return-from repl-inspect
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-inspect expression)
       (nrepl-inspect expression)))

(defun repl-describe (symbol)
  "Describe a symbol via the connected REPL."
  (unless *repl-connected*
    (return-from repl-describe
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-describe symbol)
       (nrepl-describe symbol)))

(defun repl-completions (prefix &optional (package "CL-USER"))
  "Get symbol completions via the connected REPL."
  (unless *repl-connected*
    (return-from repl-completions
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-completions prefix package)
       (nrepl-completions prefix)))

(defun repl-doc (symbol)
  "Get documentation for a symbol."
  (unless *repl-connected*
    (return-from repl-doc
      (list :error t :message "Not connected to any REPL")))

   (if (eq *repl-type* :swank)
       (mcp-swank-autodoc symbol)
       (nrepl-doc symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified Debugger Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repl-frame-locals (frame &optional (thread :repl-thread))
  "Get local variables for a frame."
  (unless *repl-connected*
    (return-from repl-frame-locals
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-frame-locals frame thread)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Frame locals not implemented for nrepl")))

(defun repl-step (frame)
  "Step into next expression in FRAME."
  (unless *repl-connected*
    (return-from repl-step
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-step frame)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Stepping not implemented for nrepl")))

(defun repl-next (frame)
  "Step over next expression in FRAME."
  (unless *repl-connected*
    (return-from repl-next
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-next frame)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Next not implemented for nrepl")))

(defun repl-out (frame)
  "Step out of current frame."
  (unless *repl-connected*
    (return-from repl-out
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-out frame)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Out not implemented for nrepl")))

(defun repl-continue ()
  "Continue execution from debugger."
  (unless *repl-connected*
    (return-from repl-continue
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-continue)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Continue not implemented for nrepl")))

(defun repl-get-restarts (&optional (frame 0))
  "Get available restarts for FRAME (default 0 = current/top frame)."
  (unless *repl-connected*
    (return-from repl-get-restarts
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-get-restarts frame)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Get restarts not implemented for nrepl")))

(defun repl-invoke-restart (restart-index)
  "Invoke the Nth restart (1-based index)."
  (unless *repl-connected*
    (return-from repl-invoke-restart
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (swank-invoke-restart restart-index)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Invoke restart not implemented for nrepl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified Breakpoint Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repl-set-breakpoint (function &key condition hit-count thread)
  "Set a breakpoint on FUNCTION."
  (unless *repl-connected*
    (return-from repl-set-breakpoint
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-set-breakpoint function :condition condition :hit-count hit-count :thread thread)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Set breakpoint not implemented for nrepl")))

(defun repl-remove-breakpoint (breakpoint-id)
  "Remove breakpoint by ID."
  (unless *repl-connected*
    (return-from repl-remove-breakpoint
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-remove-breakpoint breakpoint-id)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Remove breakpoint not implemented for nrepl")))

(defun repl-list-breakpoints ()
  "List all breakpoints."
  (unless *repl-connected*
    (return-from repl-list-breakpoints
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-list-breakpoints)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "List breakpoints not implemented for nrepl")))

(defun repl-toggle-breakpoint (breakpoint-id)
  "Toggle breakpoint enabled state."
  (unless *repl-connected*
    (return-from repl-toggle-breakpoint
      (list :error t :message "Not connected to any REPL")))

  (if (eq *repl-type* :swank)
      (mcp-swank-toggle-breakpoint breakpoint-id)
      ;; For nrepl, we might need to implement this differently
      (list :error t :message "Toggle breakpoint not implemented for nrepl")))

;;; ============================================================
;;; Help
;;; ============================================================

(defun repl-help ()
  "Get help on available unified REPL tools."
  (list :type *repl-type*
        :connected *repl-connected*
        :tools (list
               (list :name "repl_connect" :description "Connect to any REPL (Swank/nrepl)")
               (list :name "repl_disconnect" :description "Disconnect from REPL")
               (list :name "repl_status" :description "Check connection status")
               (list :name "repl_eval" :description "Evaluate Lisp code")
               (list :name "repl_compile" :description "Compile Lisp code")
               (list :name "repl_threads" :description "List threads")
               (list :name "repl_abort" :description "Abort/interrupt evaluation")
               (list :name "repl_backtrace" :description "Get backtrace")
               (list :name "repl_frame_locals" :description "Get frame local variables")
               (list :name "repl_inspect" :description "Inspect object")
               (list :name "repl_describe" :description "Describe symbol")
               (list :name "repl_completions" :description "Get completions")
               (list :name "repl_doc" :description "Get documentation")
               (list :name "repl_get_restarts" :description "Get available restarts")
               (list :name "repl_invoke_restart" :description "Invoke restart by index")
               (list :name "repl_step" :description "Step into next expression")
               (list :name "repl_next" :description "Step over next expression")
               (list :name "repl_out" :description "Step out of current frame")
               (list :name "repl_continue" :description "Continue execution")
               (list :name "repl_set_breakpoint" :description "Set breakpoint on function")
               (list :name "repl_remove_breakpoint" :description "Remove breakpoint by ID")
               (list :name "repl_list_breakpoints" :description "List all breakpoints")
               (list :name "repl_toggle_breakpoint" :description "Toggle breakpoint enabled"))
        :examples (list
                  (list :auto-detect "repl_connect" :port 4005)
                  (list :explicit-swank "repl_connect" :type :swank :port 4005)
                  (list :explicit-nrepl "repl_connect" :type :nrepl :port 7888))))
