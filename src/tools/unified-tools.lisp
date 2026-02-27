;;;; src/tools/unified-tools.lisp
;;;; Unified REPL tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "repl_connect"
  "Connect to Swank REPL. PREREQUISITE: Start Swank in SBCL first: (ql:quickload :swank) (swank:create-server :port 4006). Default port 4006."
  :input-schema (list :type "string" :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when type (validate-choice "type" type '("swank")))
               (when host (validate-string "host" host))
               (when port (validate-integer "port" port :min 1 :max 65535)))
  :body (cl-tron-mcp/unified:repl-connect :type type :host host :port port))

(define-simple-tool "repl_disconnect"
  "Disconnect from the current REPL. The Lisp session continues running - only the MCP connection is closed."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-disconnect)

(define-simple-tool "repl_status"
  "Check REPL connection status and type. Shows :connected, :type (:swank), :host, and :port. Use to verify connection before using other REPL tools."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-status)

(define-validated-tool "repl_eval"
  "Evaluate Lisp code in the connected REPL session. REQUIRES: repl_connect first. Code runs in a persistent session - all state (variables, functions, packages) is preserved across calls."
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/unified:repl-eval :code code :package package))

(define-validated-tool "repl_compile"
  "Compile and load Lisp code in the connected REPL. REQUIRES: repl_connect first. Use for hot-reloading function definitions without restarting the session."
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package))
               (when filename (validate-string "filename" filename)))
  :body (cl-tron-mcp/unified:repl-compile :code code :package package :filename filename))

(define-simple-tool "repl_threads"
  "List all threads in the connected REPL. REQUIRES: repl_connect first. Shows thread names, status, and IDs for debugging concurrency issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-threads)

(define-simple-tool "repl_abort"
  "Abort/interrupt evaluation in the connected REPL. REQUIRES: repl_connect first. Use when a computation is stuck or taking too long."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-abort)

(define-simple-tool "repl_backtrace"
  "Get the call stack from the connected REPL. REQUIRES: repl_connect first. Use after an error to see where it occurred in your code."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-backtrace)

(define-validated-tool "repl_inspect"
  "Inspect an object in the connected REPL. REQUIRES: repl_connect first. EXPRESSION is evaluated - use quoted symbols for variables."
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/unified:repl-inspect :expression expression))

(define-validated-tool "repl_describe"
  "Describe a symbol in the connected REPL. REQUIRES: repl_connect first. Shows documentation, argument list, and type information."
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/unified:repl-describe :symbol symbol))

(define-validated-tool "repl_completions"
  "Get symbol completions in the connected REPL. REQUIRES: repl_connect first. Useful for discovering available functions and variables."
  :input-schema (list :prefix "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "prefix" prefix :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/unified:repl-completions :prefix prefix :package package))

(define-validated-tool "repl_doc"
  "Get documentation for a symbol in the connected REPL. REQUIRES: repl_connect first. Returns docstring and argument list."
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/unified:repl-doc :symbol symbol))

(define-validated-tool "repl_frame_locals"
  "Get local variables for a stack frame. REQUIRES: repl_connect and an active debugger session. Use after an error to inspect variable values at the point of failure."
  :input-schema (list :frame "integer" :thread "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when thread (validate-string "thread" thread)))
  :body (cl-tron-mcp/unified:repl-frame-locals :frame frame :thread thread))

(define-validated-tool "repl_step"
  "Step into the next expression. REQUIRES: repl_connect and an active stepping context (entered via (step ...)). Shows the next form to be evaluated."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-step :frame frame))

(define-validated-tool "repl_next"
  "Step over the next expression. REQUIRES: repl_connect and an active stepping context. Evaluates the next form without stepping into function calls."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-next :frame frame))

(define-validated-tool "repl_out"
  "Step out of the current frame. REQUIRES: repl_connect and an active stepping context. Finishes the current function and stops at the return point."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-out :frame frame))

(define-simple-tool "repl_continue"
  "Continue execution from the debugger. REQUIRES: repl_connect and an active debugger session. Resumes normal execution."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-continue)

(define-validated-tool "repl_get_restarts"
  "Get available restarts for error recovery. REQUIRES: repl_connect and an active debugger session. Shows options like ABORT, RETRY, USE-VALUE."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0)))
  :body (cl-tron-mcp/unified:repl-get-restarts :frame frame))

(define-validated-tool "repl_invoke_restart"
  "Invoke a restart by index to recover from an error. REQUIRES: repl_connect and an active debugger session. Use repl_get_restarts to see available options. Index is 1-based."
  :input-schema (list :restartIndex "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-integer "restart_index" restart_index :required t :min 1))
  :body (cl-tron-mcp/unified:repl-invoke-restart :restart-index restart_index))

(define-validated-tool "repl_set_breakpoint"
  "Set a breakpoint on a function via the connected REPL"
  :input-schema (list :function "string" :condition "string" :hitCount "integer" :thread "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "function" function :required t)
               (when condition (validate-string "condition" condition))
               (when hit_count (validate-integer "hit_count" hit_count :min 0))
               (when thread (validate-string "thread" thread)))
  :body (cl-tron-mcp/unified:repl-set-breakpoint :function function :condition condition :hit-count hit_count :thread thread))

(define-validated-tool "repl_remove_breakpoint"
  "Remove a breakpoint by ID via the connected REPL"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/unified:repl-remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_list_breakpoints"
  "List all breakpoints via the connected REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-list-breakpoints)

(define-validated-tool "repl_toggle_breakpoint"
  "Toggle breakpoint enabled state via the connected REPL"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/unified:repl-toggle-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "repl_help"
  "Get help on available unified REPL tools"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/unified:repl-help)