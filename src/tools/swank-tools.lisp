;;;; src/tools/swank-tools.lisp
;;;; Swank integration tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "swank_connect"
  "Connect to Swank server"
  :input-schema (list :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-connect.md"
  :validation ((when host (validate-string "host" host))
               (when port (validate-integer "port" port :min 1 :max 65535)))
  :body (cl-tron-mcp/swank:swank-connect :host host :port port))

(define-simple-tool "swank_disconnect"
  "Disconnect from Swank"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-disconnect.md"
  :function cl-tron-mcp/swank:swank-disconnect)

(define-simple-tool "swank_status"
  "Get Swank connection status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-status.md"
  :function cl-tron-mcp/swank:swank-status)

(define-validated-tool "swank_eval"
  "Evaluate code in SBCL"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-eval.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/swank:mcp-swank-eval :code code :package package))

(define-validated-tool "swank_compile"
  "Compile and load code"
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-compile.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package))
               (when filename (validate-string "filename" filename)))
  :body (cl-tron-mcp/swank:mcp-swank-compile :code code :package package :filename filename))

(define-simple-tool "swank_threads"
  "List all SBCL threads"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-threads.md"
  :function cl-tron-mcp/swank:mcp-swank-threads)

(define-validated-tool "swank_abort"
  "Abort a thread"
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-abort.md"
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/swank:mcp-swank-abort :thread-id thread_id))

(define-simple-tool "swank_interrupt"
  "Interrupt current thread"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-interrupt.md"
  :function cl-tron-mcp/swank:mcp-swank-interrupt)

(define-simple-tool "swank_backtrace"
  "Get call stack"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-backtrace.md"
  :function cl-tron-mcp/swank:mcp-swank-backtrace)

(define-validated-tool "swank_inspect"
  "Inspect object in SBCL"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-inspect.md"
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/swank:mcp-swank-inspect :expression expression))

(define-validated-tool "swank_describe"
  "Describe symbol in SBCL"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-describe.md"
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/swank:mcp-swank-describe :expression expression))

(define-validated-tool "swank_autodoc"
  "Get argument list and docs"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-autodoc.md"
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/swank:mcp-swank-autodoc :symbol symbol))

(define-validated-tool "swank_completions"
  "Get symbol completions"
  :input-schema (list :symbol "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-completions.md"
  :validation ((validate-string "symbol" symbol :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/swank:mcp-swank-completions :symbol symbol :package package))

(define-simple-tool "swank_get_restarts"
  "Get available restarts"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-get-restarts.md"
  :function cl-tron-mcp/swank:swank-get-restarts)

(define-validated-tool "swank_invoke_restart"
  "Invoke a restart"
  :input-schema (list :restart_index "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-invoke-restart.md"
  :validation ((validate-integer "restart_index" restart_index :required t :min 1))
  :body (cl-tron-mcp/swank:swank-invoke-restart :restart-index restart_index))

(define-simple-tool "swank_continue"
  "Continue from debugger"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-continue.md"
  :function cl-tron-mcp/swank:swank-continue)

(define-simple-tool "swank_step"
  "Step into next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-step.md"
  :function cl-tron-mcp/swank:swank-step)

(define-simple-tool "swank_next"
  "Step over next expression"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-next.md"
  :function cl-tron-mcp/swank:swank-next)

(define-simple-tool "swank_out"
  "Step out of current frame"
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-out.md"
  :function cl-tron-mcp/swank:swank-out)

(define-simple-tool "swank_debugger_state"
  "Get debugger state"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-debugger-state.md"
  :function cl-tron-mcp/swank:swank-debugger-state)