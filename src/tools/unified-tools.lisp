;;;; src/tools/unified-tools.lisp
;;;; Unified REPL tool registrations

(in-package :cl-tron-mcp/tools)

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
  (cl-tron-mcp/resources:make-error-full "REPL_NOT_CONNECTED"))

(defun make-already-connected-error ()
  "Return an error when already connected."
  (cl-tron-mcp/resources:make-error-with-hint "REPL_ALREADY_CONNECTED"
                                         :details (list :type *repl-type*
                                                        :host *repl-host*
                                                        :port *repl-port*)))

;;; ============================================================
;;; Unified Connection
;;; ============================================================

(defun repl-connect (&key (type :auto) (host "127.0.0.1") (port (cl-tron-mcp/config:get-config :swank-port)))
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
      (cl-tron-mcp/resources:make-error-with-hint "NREPL_NOT_SUPPORTED")))

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
       (cl-tron-mcp/resources:make-error-with-hint "REPL_DETECTION_FAILED"
                                              :details (list :host host :port port))))))

(defun auto-detect-repl (host port)
  "Try to detect Swank by probing the port with a raw TCP connect.
Does NOT do a full Swank handshake (which would consume a connection
slot on :spawn-style servers)."
  (handler-case
      (let ((sock (usocket:socket-connect host port :timeout 2
                                                    :element-type '(unsigned-byte 8))))
        (ignore-errors (usocket:socket-close sock))
        :swank)
    (error () nil)))

(defun repl-disconnect ()
  "Disconnect from the current REPL."
  (when *repl-connected*
    (swank-disconnect)
    (setq *repl-connected* nil
          *repl-type* nil))
  (list :success t :message "Disconnected from REPL"))

(defun repl-connected-p ()
  "Return T if currently connected to a REPL."
  (and *repl-connected* t))

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
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_CODE_PARAMETER"
                                             :details (list :tool "repl_eval"))))
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
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_CODE_PARAMETER"
                                             :details (list :tool "repl_compile"))))
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
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_EXPRESSION_PARAMETER"
                                             :details (list :tool "repl_inspect"))))
  (unless *repl-connected*
    (return-from repl-inspect (make-not-connected-error)))
  (mcp-swank-inspect :expression expression))

(defun repl-describe (&key symbol)
  "Describe a symbol via the connected REPL."
  (unless symbol
    (return-from repl-describe
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_SYMBOL_PARAMETER"
                                             :details (list :tool "repl_describe"))))
  (unless *repl-connected*
    (return-from repl-describe (make-not-connected-error)))
  (mcp-swank-describe :expression symbol))

(defun repl-completions (&key prefix (package "CL-USER"))
  "Get symbol completions via the connected REPL."
  (unless prefix
    (return-from repl-completions
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_PREFIX_PARAMETER"
                                             :details (list :tool "repl_completions"))))
  (unless *repl-connected*
    (return-from repl-completions (make-not-connected-error)))
  (mcp-swank-completions :prefix prefix :package package))

(defun repl-doc (&key symbol)
  "Get documentation for a symbol."
  (unless symbol
    (return-from repl-doc
      (cl-tron-mcp/resources:make-error-with-hint "INVALID_SYMBOL_PARAMETER"
                                             :details (list :tool "repl_doc"))))
  (unless *repl-connected*
    (return-from repl-doc (make-not-connected-error)))
  (mcp-swank-autodoc :symbol symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unified Debugger Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repl-frame-locals (&key frame thread)
  "Get local variables for a frame.
THREAD defaults to NIL; the swank layer then targets the thread suspended
in the debugger, the only thread where frame locals exist (FD-009 bug #9)."
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

(defun repl-get-restarts (&key frame)
  "Get available restarts for current debugger state."
  (declare (ignore frame))
  (unless *repl-connected*
    (return-from repl-get-restarts (make-not-connected-error)))
  (swank-get-restarts))

(defun repl-invoke-restart (&key restart_index)
  "Invoke the Nth restart (0-based index: 0 = the first/CONTINUE restart in
the debugger's restart list, matching the order shown in the :debug payload)."
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

(define-validated-tool "repl_connect"
  "Connect to Swank REPL"
  :input-schema (list :type "string" :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-connect.md"
  :validation ((when type (validate-choice "type" type '("swank")))
               (when host (validate-string "host" host))
               (when port (validate-integer "port" port :min 1 :max 65535)))
  :body (repl-connect
           :type (if type (intern (string-upcase type) :keyword) :auto)
           :host (or host "127.0.0.1")
           :port (or port (cl-tron-mcp/config:get-config :swank-port))))

(define-simple-tool "repl_disconnect"
  "Disconnect from REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-disconnect.md"
  :function repl-disconnect)

(define-simple-tool "repl_status"
  "Check REPL connection status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-status.md"
  :function repl-status)

(define-validated-tool "repl_eval"
  "Evaluate Lisp code in REPL"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-eval.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (repl-eval :code code :package (or package "CL-USER")))

(define-validated-tool "repl_compile"
  "Compile and load Lisp code"
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-compile.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package))
               (when filename (validate-string "filename" filename)))
  :body (repl-compile :code code :package (or package "CL-USER") :filename (or filename "repl")))

(define-simple-tool "repl_threads"
  "List all REPL threads"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-threads.md"
  :function repl-threads)

(define-simple-tool "repl_abort"
  "Abort/interrupt REPL evaluation"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-abort.md"
  :function repl-abort)

(define-simple-tool "repl_backtrace"
  "Get REPL call stack"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-backtrace.md"
  :function repl-backtrace)

(define-validated-tool "repl_inspect"
  "Inspect object in REPL"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-inspect.md"
  :validation ((validate-string "expression" expression :required t))
  :body (repl-inspect :expression expression))

(define-validated-tool "repl_describe"
  "Describe symbol in REPL"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-describe.md"
  :validation ((validate-string "symbol" symbol :required t))
  :body (repl-describe :symbol symbol))

(define-validated-tool "repl_completions"
  "Get symbol completions"
  :input-schema (list :prefix "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-completions.md"
  :validation ((validate-string "prefix" prefix :required t)
               (when package (validate-package-name "package" package)))
  :body (repl-completions :prefix prefix :package package))

(define-validated-tool "repl_doc"
  "Get symbol documentation"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-doc.md"
  :validation ((validate-string "symbol" symbol :required t))
  :body (repl-doc :symbol symbol))

(define-validated-tool "repl_frame_locals"
  "Get frame local variables"
  :input-schema (list :frame "integer" :thread "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-frame-locals.md"
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when thread (validate-string "thread" thread)))
  :body (repl-frame-locals :frame frame :thread thread))

(define-validated-tool "repl_step"
  "Step into next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-step.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (repl-step :frame frame))

(define-validated-tool "repl_next"
  "Step over next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-next.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (repl-next :frame frame))

(define-validated-tool "repl_out"
  "Step out of current frame"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-out.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (repl-out :frame frame))

(define-simple-tool "repl_continue"
  "Continue from debugger"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-continue.md"
  :function repl-continue)

(define-validated-tool "repl_get_restarts"
  "Get available restarts"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-get-restarts.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (repl-get-restarts :frame frame))

(define-validated-tool "repl_invoke_restart"
  "Invoke a restart by index (0-based; 0 = the first/CONTINUE restart)"
  :input-schema (list :restart_index "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-invoke-restart.md"
  :validation ((validate-integer "restart_index" restart_index :required t :min 0))
  :body (repl-invoke-restart :restart_index restart_index))

(define-validated-tool "repl_set_breakpoint"
  "Set a breakpoint"
  :input-schema (list :function "string" :condition "string" :hitCount "integer" :thread "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-set-breakpoint.md"
  :validation ((validate-string "function" function :required t)
               (when condition (validate-string "condition" condition))
               (when hit_count (validate-integer "hit_count" hit_count :min 0))
               (when thread (validate-string "thread" thread)))
  :body (repl-set-breakpoint :function function :condition condition :hit-count hit_count :thread thread))

(define-validated-tool "repl_remove_breakpoint"
  "Remove a breakpoint"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-remove-breakpoint.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (repl-remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_list_breakpoints"
  "List all breakpoints"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-list-breakpoints.md"
  :function repl-list-breakpoints)

(define-validated-tool "repl_toggle_breakpoint"
  "Toggle breakpoint state"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-toggle-breakpoint.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (repl-toggle-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_help"
  "Get help on REPL tools"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-help.md"
  :function repl-help)