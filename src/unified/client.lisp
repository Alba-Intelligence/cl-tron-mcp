;;;; src/unified/client.lisp - Unified REPL interface for CL-TRON-MCP
;;;;
;;;; This module provides a unified interface for connecting to
;;;; Swank (Slime/Portacle/Sly) servers.
;;;;
;;;; Error Messages:
;;;; All error messages include :hint fields with actionable guidance
;;;; to help AI agents understand how to resolve common issues.
;;;;
;;;; See: docs/architecture.md for overall architecture

(in-package :cl-tron-mcp/unified)

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *repl-connected* nil
  "Connection status")

(defvar *repl-type* nil
  "Current REPL type (always :swank)")

(defvar *repl-port* nil
  "Connected port")

(defvar *repl-host* nil
  "Connected host")

;;; ============================================================
;;; Error Helpers
;;; ============================================================

(defun make-not-connected-error ()
  "Return a helpful error when not connected to a REPL.
Includes hints for starting Swank and connecting."
  (list :error t
        :message "Not connected to any REPL"
        :hint "Run repl_connect first. Example: repl_connect :port 4005"
        :setup "To start Swank in SBCL: (ql:quickload :swank) (swank:create-server :port 4005 :dont-close t)"
        :docs "See prompts/get 'getting-started' for step-by-step instructions"))

(defun make-already-connected-error ()
  "Return an error when already connected."
  (list :error t
        :message "Already connected to a REPL"
        :hint "Use repl_disconnect first if you want to connect to a different server"
        :current (list :type *repl-type* :host *repl-host* :port *repl-port*)))

;;; ============================================================
;;; Unified Connection
;;; ============================================================

(defun repl-connect (&key (type :auto) (host "127.0.0.1") (port 4005))
  "Connect to a Lisp REPL (Swank).

   Usage:
     ;; Auto-detect Swank on port
     (repl-connect :port 4005)

     ;; Explicit Swank (Slime, Portacle, Sly)
     (repl-connect :type :swank :port 4005)

   Returns: Connection status with :type :swank"
  (when *repl-connected*
    (return-from repl-connect (make-already-connected-error)))

  (when (eq type :nrepl)
    (return-from repl-connect
      (list :error t
            :message "nrepl is no longer supported; use Swank"
            :hint "Use repl_connect :type :swank :port 4005 or start Swank: (ql:quickload :swank) (swank:create-server :port 4005)")))

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

      (t
       (list :error t
             :message "Could not detect Swank"
             :hint "Ensure Swank server is running on the specified port"
             :setup "For Swank: (ql:quickload :swank) (swank:create-server :port 4005)"
             :tried (list :host host :port port))))))

(defun auto-detect-repl (host port)
  "Try to detect Swank by connecting."
  (handler-case
      (let ((swank-result (swank-connect :host host :port port :timeout 2)))
        (when (getf swank-result :success)
          (swank-disconnect)
          (return-from auto-detect-repl :swank)))
    (error nil))
  nil)

(defun repl-disconnect ()
  "Disconnect from the current REPL."
  (when *repl-connected*
    (swank-disconnect)
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

(defun repl-eval (&key code (package "CL-USER"))
  "Evaluate Lisp code via the connected REPL.

   Example:
     (repl-eval :code \"(+ 10 20)\")
     (repl-eval :code \"(defun hello () 'world)\" :package \"MY-PACKAGE\")

   Returns: Evaluation result"
  (unless code
    (return-from repl-eval
      (list :error t
            :message "code is required"
            :hint "Provide a Lisp expression as a string, e.g., :code \"(+ 1 2)\"")))
  (unless *repl-connected*
    (return-from repl-eval (make-not-connected-error)))

  (let ((result (mcp-swank-eval :code code :package package)))
    (when (getf result :value)
      (setf (getf result :result) (getf result :value)))
    result))

(defun repl-compile (&key code (package "CL-USER") (filename "repl"))
  "Compile Lisp code via the connected REPL."
  (unless code
    (return-from repl-compile
      (list :error t
            :message "code is required"
            :hint "Provide Lisp code as a string, e.g., :code \"(defun foo () t)\"")))
  (unless *repl-connected*
    (return-from repl-compile (make-not-connected-error)))

  (mcp-swank-compile :code code :package package :filename filename))

;;; ============================================================
;;; Unified Operations
;;; ============================================================

(defun repl-threads ()
  "List all threads in the connected SBCL."
  (unless *repl-connected*
    (return-from repl-threads (make-not-connected-error)))
  (mcp-swank-threads))

(defun repl-abort (&key thread)
  "Abort a thread or interrupt evaluation."
  (unless *repl-connected*
    (return-from repl-abort (make-not-connected-error)))
  (mcp-swank-abort :thread-id thread))

(defun repl-backtrace ()
  "Get the current backtrace."
  (unless *repl-connected*
    (return-from repl-backtrace (make-not-connected-error)))
  (mcp-swank-backtrace))

(defun repl-inspect (&key expression)
  "Inspect an object via the connected REPL."
  (unless expression
    (return-from repl-inspect
      (list :error t
            :message "expression is required"
            :hint "Provide an expression that evaluates to an object, e.g., :expression \"*package*\"")))
  (unless *repl-connected*
    (return-from repl-inspect (make-not-connected-error)))
  (mcp-swank-inspect :expression expression))

(defun repl-describe (&key symbol)
  "Describe a symbol via the connected REPL."
  (unless symbol
    (return-from repl-describe
      (list :error t
            :message "symbol is required"
            :hint "Provide a symbol name, e.g., :symbol \"mapcar\"")))
  (unless *repl-connected*
    (return-from repl-describe (make-not-connected-error)))
  (mcp-swank-describe :expression symbol))

(defun repl-completions (&key prefix (package "CL-USER"))
  "Get symbol completions via the connected REPL."
  (unless prefix
    (return-from repl-completions
      (list :error t
            :message "prefix is required"
            :hint "Provide a symbol prefix, e.g., :prefix \"mak\"")))
  (unless *repl-connected*
    (return-from repl-completions (make-not-connected-error)))
  (mcp-swank-completions :prefix prefix :package package))

(defun repl-doc (&key symbol)
  "Get documentation for a symbol."
  (unless symbol
    (return-from repl-doc
      (list :error t
            :message "symbol is required"
            :hint "Provide a symbol name, e.g., :symbol \"format\"")))
  (unless *repl-connected*
    (return-from repl-doc (make-not-connected-error)))
  (mcp-swank-autodoc :symbol symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified Debugger Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repl-frame-locals (&key frame (thread :repl-thread))
  "Get local variables for a frame."
  (unless *repl-connected*
    (return-from repl-frame-locals (make-not-connected-error)))
  (mcp-swank-frame-locals :frame frame :thread thread))

(defun repl-step (&key frame)
  "Step into next expression in FRAME."
  (unless *repl-connected*
    (return-from repl-step (make-not-connected-error)))
  (swank-step :frame-index frame))

(defun repl-next (&key frame)
  "Step over next expression in FRAME."
  (unless *repl-connected*
    (return-from repl-next (make-not-connected-error)))
  (swank-next :frame-index frame))

(defun repl-out (&key frame)
  "Step out of current frame."
  (unless *repl-connected*
    (return-from repl-out (make-not-connected-error)))
  (swank-out :frame-index frame))

(defun repl-continue ()
  "Continue execution from debugger."
  (unless *repl-connected*
    (return-from repl-continue (make-not-connected-error)))
  (swank-continue))

(defun repl-get-restarts ()
  "Get available restarts for current debugger state."
  (unless *repl-connected*
    (return-from repl-get-restarts (make-not-connected-error)))
  (swank-get-restarts))

(defun repl-invoke-restart (&key restart_index)
  "Invoke the Nth restart (1-based index)."
  (unless *repl-connected*
    (return-from repl-invoke-restart (make-not-connected-error)))
  (swank-invoke-restart :restart_index restart_index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified Breakpoint Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repl-set-breakpoint (&key function condition hit_count thread)
  "Set a breakpoint on FUNCTION."
  (unless *repl-connected*
    (return-from repl-set-breakpoint (make-not-connected-error)))
  (mcp-swank-set-breakpoint :function function :condition condition :hit-count hit_count :thread thread))

(defun repl-remove-breakpoint (&key breakpoint_id)
  "Remove breakpoint by ID."
  (unless *repl-connected*
    (return-from repl-remove-breakpoint (make-not-connected-error)))
  (mcp-swank-remove-breakpoint :breakpoint-id breakpoint_id))

(defun repl-list-breakpoints ()
  "List all breakpoints."
  (unless *repl-connected*
    (return-from repl-list-breakpoints (make-not-connected-error)))
  (mcp-swank-list-breakpoints))

(defun repl-toggle-breakpoint (&key breakpoint_id)
  "Toggle breakpoint enabled state."
  (unless *repl-connected*
    (return-from repl-toggle-breakpoint (make-not-connected-error)))
  (mcp-swank-toggle-breakpoint :breakpoint-id breakpoint_id))

;;; ============================================================
;;; Help
;;; ============================================================

(defun repl-help ()
  "Get help on available unified REPL tools."
  (list :type *repl-type*
        :connected *repl-connected*
        :tools (list
               (list :name "repl_connect" :description "Connect to Swank REPL")
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
                  (list :explicit-swank "repl_connect" :type :swank :port 4005))))
