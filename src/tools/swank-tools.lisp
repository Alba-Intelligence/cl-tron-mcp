;;;; src/tools/swank-tools.lisp
;;;; Swank integration tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "swank_connect"
  "Connect to a running SBCL with Swank loaded. PREREQUISITE: Start Swank in SBCL first with (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t). Default port is 4006. Returns connection status."
  :input-schema (list :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when host (validate-string "host" host))
               (when port (validate-integer "port" port :min 1 :max 65535)))
  :body (cl-tron-mcp/swank:swank-connect :host host :port port))

(define-simple-tool "swank_disconnect"
  "Disconnect from the Swank server. The SBCL session continues running - only the MCP connection is closed."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-disconnect)

(define-simple-tool "swank_status"
  "Get the current Swank connection status. Shows if connected, reader thread status, and event processor status. Use to verify connection before using other Swank tools."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-status)

(define-validated-tool "swank_eval"
  "Evaluate Lisp code in the connected SBCL session. REQUIRES: swank_connect first. Code runs in a persistent session - state is preserved across calls. Use for testing, debugging, and hot-patching code."
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/swank:mcp-swank-eval :code code :package package))

(define-validated-tool "swank_compile"
  "Compile and load Lisp code in the connected SBCL. REQUIRES: swank_connect first. Use for hot-reloading function definitions. The filename parameter helps with debugging messages."
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package))
               (when filename (validate-string "filename" filename)))
  :body (cl-tron-mcp/swank:mcp-swank-compile :code code :package package :filename filename))

(define-simple-tool "swank_threads"
  "List all threads in the connected SBCL. REQUIRES: swank_connect first. Shows thread names, status, and IDs for use with other thread tools."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:mcp-swank-threads)

(define-validated-tool "swank_abort"
  "Abort a specific thread in the connected SBCL. REQUIRES: swank_connect first. Useful when a thread is stuck or in an error condition."
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/swank:mcp-swank-abort :thread-id thread_id))

(define-simple-tool "swank_interrupt"
  "Interrupt the current thread's execution in the connected SBCL. REQUIRES: swank_connect first. Triggers the debugger for long-running computations."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :function cl-tron-mcp/swank:mcp-swank-interrupt)

(define-simple-tool "swank_backtrace"
  "Get the current call stack from the connected SBCL. REQUIRES: swank_connect first. Use after swank_eval triggers an error to see where it occurred. Shows function names and source locations."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:mcp-swank-backtrace)

(define-validated-tool "swank_inspect"
  "Inspect an object in the connected SBCL. REQUIRES: swank_connect first. EXPRESSION is evaluated, so use quoted symbols for variables: '*my-var*' or expressions: '(make-hash-table)'."
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/swank:mcp-swank-inspect :expression expression))

(define-validated-tool "swank_describe"
  "Describe a symbol in the connected SBCL. REQUIRES: swank_connect first. Shows documentation, argument list, and source location."
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "expression" expression :required t))
  :body (cl-tron-mcp/swank:mcp-swank-describe :expression expression))

(define-validated-tool "swank_autodoc"
  "Get argument list and documentation for a symbol. REQUIRES: swank_connect first. Example: 'mapcar' shows (function list &rest more-lists)."
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "symbol" symbol :required t))
  :body (cl-tron-mcp/swank:mcp-swank-autodoc :symbol symbol))

(define-validated-tool "swank_completions"
  "Get symbol completions in the connected SBCL. REQUIRES: swank_connect first. Example: prefix 'mak' returns (make-array make-hash-table make-instance ...)."
  :input-schema (list :symbol "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "symbol" symbol :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/swank:mcp-swank-completions :symbol symbol :package package))

(define-simple-tool "swank_get_restarts"
  "Get available restarts when in the debugger. REQUIRES: swank_connect first. Use after swank_eval triggers an error. Shows recovery options like ABORT, RETRY, USE-VALUE, CONTINUE."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-get-restarts)

(define-validated-tool "swank_invoke_restart"
  "Invoke a restart by index to recover from an error. REQUIRES: swank_connect first. Use swank_get_restarts to see available options. Index is 1-based."
  :input-schema (list :restart_index "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-integer "restart_index" restart_index :required t :min 1))
  :body (cl-tron-mcp/swank:swank-invoke-restart :restart-index restart_index))

(define-simple-tool "swank_continue"
  "Continue execution from the debugger. REQUIRES: swank_connect first. Resumes normal execution after an error (if the condition is continuable)."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-continue)

(define-simple-tool "swank_step"
  "Step into the next expression in the debugger. REQUIRES: swank_connect first. Use when in a stepping context (entered via (step ...)). Shows the next form to be evaluated."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-step)

(define-simple-tool "swank_next"
  "Step over the next expression in the debugger. REQUIRES: swank_connect first. Evaluates the next form without stepping into function calls."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-next)

(define-simple-tool "swank_out"
  "Step out of the current frame in the debugger. REQUIRES: swank_connect first. Finishes the current function and stops at the return point."
  :input-schema (list :frame "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-out)

(define-simple-tool "swank_debugger_state"
  "Get the current debugger state: which thread is in the debugger, the debugger level, and whether debugging is active. REQUIRES: swank_connect first."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/swank:swank-debugger-state)