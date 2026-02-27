;;;; src/tools/unified-tools.lisp
;;;; Unified REPL tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "repl_connect"
  "Connect to Swank REPL"
  :input-schema (list :type "string" :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-connect.md"
  :validation ((when type (validate-choice "type" type '("swank")))
               (when host (validate-string "host" host))
               (when port (validate-integer "port" port :min 1 :max 65535)))
  :body (cl-tron-mcp/unified:repl-connect :type type :host host :port port))

(define-simple-tool "repl_disconnect"
  "Disconnect from REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-disconnect.md"
  :function cl-tron-mcp/unified:repl-disconnect)

(define-simple-tool "repl_status"
  "Check REPL connection status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-status.md"
  :function cl-tron-mcp/unified:repl-status)

(define-validated-tool "repl_eval"
  "Evaluate Lisp code in REPL"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-eval.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/unified:repl-eval :code code :package package))

(define-validated-tool "repl_compile"
  "Compile and load Lisp code"
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-compile.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package))
               (when filename (validate-string "filename" filename)))
  :body (cl-tron-mcp/unified:repl-compile :code code :package package :filename filename))

(define-simple-tool "repl_threads"
  "List all REPL threads"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-threads.md"
  :function cl-tron-mcp/unified:repl-threads)

(define-simple-tool "repl_abort"
  "Abort/interrupt REPL evaluation"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-abort.md"
  :function cl-tron-mcp/unified:repl-abort)

(define-simple-tool "repl_backtrace"
  "Get REPL call stack"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-backtrace.md"
  :function cl-tron-mcp/unified:repl-backtrace)

(define-validated-tool "repl_inspect"
  "Inspect object in REPL"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-inspect.md"
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/unified:repl-inspect :expression expression))

(define-validated-tool "repl_describe"
  "Describe symbol in REPL"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-describe.md"
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/unified:repl-describe :symbol symbol))

(define-validated-tool "repl_completions"
  "Get symbol completions"
  :input-schema (list :prefix "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-completions.md"
  :validation ((validate-string "prefix" prefix :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/unified:repl-completions :prefix prefix :package package))

(define-validated-tool "repl_doc"
  "Get symbol documentation"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-doc.md"
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/unified:repl-doc :symbol symbol))

(define-validated-tool "repl_frame_locals"
  "Get frame local variables"
  :input-schema (list :frame "integer" :thread "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-frame-locals.md"
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when thread (validate-string "thread" thread)))
  :body (cl-tron-mcp/unified:repl-frame-locals :frame frame :thread thread))

(define-validated-tool "repl_step"
  "Step into next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-step.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-step :frame frame))

(define-validated-tool "repl_next"
  "Step over next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-next.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-next :frame frame))

(define-validated-tool "repl_out"
  "Step out of current frame"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-out.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-out :frame frame))

(define-simple-tool "repl_continue"
  "Continue from debugger"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-continue.md"
  :function cl-tron-mcp/unified:repl-continue)

(define-validated-tool "repl_get_restarts"
  "Get available restarts"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-get-restarts.md"
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-get-restarts :frame frame))

(define-validated-tool "repl_invoke_restart"
  "Invoke a restart"
  :input-schema (list :restartIndex "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-invoke-restart.md"
  :validation ((validate-integer "restart_index" restart_index :required t :min 1))
  :body (cl-tron-mcp/unified:repl-invoke-restart :restart-index restart_index))

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
  :body (cl-tron-mcp/unified:repl-set-breakpoint :function function :condition condition :hit-count hit_count :thread thread))

(define-validated-tool "repl_remove_breakpoint"
  "Remove a breakpoint"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-remove-breakpoint.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/unified:repl-remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_list_breakpoints"
  "List all breakpoints"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-list-breakpoints.md"
  :function cl-tron-mcp/unified:repl-list-breakpoints)

(define-validated-tool "repl_toggle_breakpoint"
  "Toggle breakpoint state"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-toggle-breakpoint.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/unified:repl-toggle-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_help"
  "Get help on REPL tools"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/repl-help.md"
  :function cl-tron-mcp/unified:repl-help)