;;;; src/tools/register-tools.lisp
;;;;
;;;; Tool registration module - loads after all tool modules are defined.
;;;; Registers all tools with the registry.

(in-package :cl-tron-mcp/tools)

;;; Inspector tools
(register-tool
 "inspect_object"
 "Inspect an object by its ID. Use when you need to examine the slots and structure of a CLOS instance or other object. Returns type, slots, and nested object IDs for further inspection."
 :input-schema (list :objectId "string" :maxDepth "integer")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_object" (function cl-tron-mcp/inspector:inspect-object))

(register-tool
 "inspect_slot"
 "Get or set a slot value on an object. Use to read or modify individual slots of a CLOS instance. Set value by providing the value parameter."
 :input-schema (list :objectId "string" :slotName "string" :value "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_slot" (function cl-tron-mcp/inspector:inspect-slot))

(register-tool
 "inspect_class"
 "Inspect a CLOS class definition. Shows superclasses, slots, and methods. Use to understand class structure before working with instances."
 :input-schema (list :className "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_class" (function cl-tron-mcp/inspector:inspect-class))

(register-tool
 "inspect_function"
 "Inspect a function definition. Shows lambda list, documentation, and source location. Use to understand how a function should be called."
 :input-schema (list :symbolName "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_function" (function cl-tron-mcp/inspector:inspect-function))

(register-tool
 "inspect_package"
 "Inspect a package and list its exported symbols. Use to discover available functions and variables in a package."
 :input-schema (list :packageName "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_package" (function cl-tron-mcp/inspector:inspect-package))

;;; REPL tools
(register-tool
 "repl_eval"
 "Evaluate Lisp code in REPL context. Use for testing, debugging, and modifying running code. Requires connection to Swank. Code runs in the persistent Lisp session."
 :input-schema (list :code "string" :package "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "repl_eval" (function cl-tron-mcp/repl:repl-eval))

;;; Monitor tools
 (register-tool
  "health_check"
  "Basic health check for the MCP server. Use to verify Tron is running and responsive. Returns server status and version."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "health_check" (function cl-tron-mcp/monitor:health-check))

 (register-tool
  "runtime_stats"
  "Get runtime statistics including memory usage, thread count, and GC info. Use to monitor system health and diagnose memory issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "runtime_stats" (function cl-tron-mcp/monitor:runtime-stats))

 (register-tool
  "gc_run"
  "Force garbage collection. Use to free memory or test GC behavior. Optionally specify generation (0-6 for SBCL)."
  :input-schema (list :generation "integer")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "gc_run" (function cl-tron-mcp/monitor:gc-run))

 (register-tool
  "system_info"
  "Get comprehensive system information including Lisp implementation, OS, and runtime details. Use for debugging environment issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "system_info" (function cl-tron-mcp/monitor:system-info))

;;; Debugger tools
(register-tool
 "debugger_frames"
 "Get debugger stack frames when an error has occurred. Use after swank_eval returns an error to see the call stack. Shows function names and source locations."
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "debugger_frames" (function cl-tron-mcp/debugger:get-debugger-frames))

(register-tool
 "debugger_restarts"
  "List available debugger restarts. Use to see recovery options after an error. Common restarts: ABORT, RETRY, USE-VALUE, CONTINUE."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
(register-tool-handler "debugger_restarts" (function cl-tron-mcp/debugger:list-restarts))

(register-tool
 "breakpoint_set"
 "Set a breakpoint on a function. Execution will pause when the function is called. Requires approval. Use for proactive debugging."
 :input-schema (list :functionName "string" :condition "string" :hitCount "integer")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "breakpoint_set" (function cl-tron-mcp/debugger:set-breakpoint))

(register-tool
 "breakpoint_remove"
 "Remove a breakpoint by its ID. Use to clear breakpoints after debugging is complete."
 :input-schema (list :breakpointId "integer")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "breakpoint_remove" (function cl-tron-mcp/debugger:remove-breakpoint))

(register-tool
 "breakpoint_list"
 "List all active breakpoints. Use to see what breakpoints are currently set."
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "breakpoint_list" (function cl-tron-mcp/debugger:list-breakpoints))

(register-tool
 "step_frame"
 "Step execution in a debugger frame. Modes: into (step into calls), over (step over calls), out (step out of current function). Use after hitting a breakpoint."
 :input-schema (list :frame "integer" :mode "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "step_frame" (function cl-tron-mcp/debugger:step-frame))

;;; Hot-reload tools
(register-tool
 "code_compile_string"
 "Compile and load Lisp code string into the running image. Use to hot-fix bugs or add functionality without restart. Requires approval. Code persists only in memory - update source files separately."
 :input-schema (list :code "string" :filename "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "code_compile_string" (function cl-tron-mcp/hot-reload:compile-and-load))

(register-tool
 "reload_system"
 "Reload an ASDF system with dependencies. Use to pick up source file changes. Force option reloads even if not changed. Requires approval."
 :input-schema (list :systemName "string" :force "boolean")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "reload_system" (function cl-tron-mcp/hot-reload:reload-system))

;;; Profiler tools
(register-tool
 "profile_start"
 "Start deterministic profiling. All function calls will be timed. Use to find performance bottlenecks. Remember to call profile_stop when done."
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "profile_start" (function cl-tron-mcp/profiler:profile-start))

(register-tool
 "profile_stop"
 "Stop profiling and return report. Call after running the code you want to profile. Returns timing data for all functions called during profiling."
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval t)
 (register-tool-handler "profile_stop" (function cl-tron-mcp/profiler:profile-stop))

 (register-tool
  "profile_report"
  "Get profiling report in specified format. Formats: flat (simple list), graph (call tree), cumulative (total time per function). Use after profile_start/stop."
  :input-schema (list :format (list :enum (list "flat" "graph" "cumulative")))
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "profile_report" (function cl-tron-mcp/profiler:profile-report))

 ;;; Tracer tools
(register-tool
 "trace_function"
 "Add trace to a function. Every call will print arguments and return value. Use to understand execution flow. Requires approval."
 :input-schema (list :functionName "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "trace_function" (function cl-tron-mcp/tracer:trace-function))

 (register-tool
  "trace_remove"
  "Remove trace from a function. Use when done debugging to stop trace output."
  :input-schema (list :functionName "string")
  :output-schema (list :type "object")
  :requires-approval t)
 (register-tool-handler "trace_remove" (function cl-tron-mcp/tracer:trace-remove))

 (register-tool
  "trace_list"
  "List all currently traced functions. Use to see what traces are active."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
(register-tool-handler "trace_list" (function cl-tron-mcp/tracer:trace-list))

;;; SBCL tools
 (register-tool
  "thread_list"
   "List all threads with their status (running, waiting, etc). Use to monitor multi-threaded applications and debug concurrency issues."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
 (register-tool-handler "thread_list" (function cl-tron-mcp/sbcl:list-threads))

 (register-tool
  "thread_inspect"
  "Get detailed information about a specific thread including name, state, and stack usage. Use to understand thread behavior."
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "thread_inspect" (function cl-tron-mcp/sbcl:inspect-thread))

 (register-tool
  "thread_backtrace"
  "Get backtrace for a specific thread. Use to see what a thread is currently doing or where it's blocked."
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "thread_backtrace" (function cl-tron-mcp/sbcl:thread-backtrace))

 ;;; Logging tools
 (register-tool
  "log_configure"
  "Configure logging level for a package. Levels: trace, debug, info, warn, error, fatal. Use to control log verbosity."
  :input-schema (list :level (list :enum (list "trace" "debug" "info" "warn" "error" "fatal")) :package "string" :appender "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "log_configure" (function cl-tron-mcp/logging:log-configure))

 (register-tool
  "log_info"
  "Log an info message. Use for general information about program execution."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "log_info" (function cl-tron-mcp/logging:log-info))

 (register-tool
  "log_debug"
  "Log a debug message. Use for detailed debugging information."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "log_debug" (function cl-tron-mcp/logging:log-debug))

 (register-tool
  "log_warn"
  "Log a warning message. Use for non-fatal issues that should be noted."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "log_warn" (function cl-tron-mcp/logging:log-warn))

 (register-tool
  "log_error"
  "Log an error message. Use for errors that don't crash the program."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "log_error" (function cl-tron-mcp/logging:log-error))

 ;;; Cross-reference tools
 (register-tool
  "who_calls"
  "Find functions that call the given symbol. Use to understand dependencies before modifying a function."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "who_calls" (function cl-tron-mcp/xref:who-calls))

 (register-tool
  "who_references"
  "Find references to the given symbol (variable references). Use to see where a variable is used."
  :input-schema (list :symbolName "string")
 :output-schema (list :type "object")
 :requires-approval nil)
 (register-tool-handler "who_references" (function cl-tron-mcp/xref:who-references))

 (register-tool
  "who_binds"
  "Find bindings of the given symbol (let bindings, function parameters). Use to see where a variable is bound."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "who_binds" (function cl-tron-mcp/xref:who-binds))

 (register-tool
   "who_sets"
   "Find setq/makunbound of the given symbol. Use to see where a variable is modified."
   :input-schema (list :symbolName "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "who_sets" (function cl-tron-mcp/xref:who-sets))

  (register-tool
  "list_callees"
  "List functions called by the given symbol"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "list_callees" (function cl-tron-mcp/xref:list-callees))

 ;;; Approval whitelist tools
 (register-tool
  "whitelist_add"
  "Add a pattern to the approval whitelist"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "whitelist_add" (function cl-tron-mcp/security:whitelist-add))

 (register-tool
  "whitelist_remove"
  "Remove a pattern from the approval whitelist"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "whitelist_remove" (function cl-tron-mcp/security:whitelist-remove))

 (register-tool
  "whitelist_clear"
  "Clear the approval whitelist"
  :input-schema (list :operation "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "whitelist_clear" (function cl-tron-mcp/security:whitelist-clear))

 (register-tool
  "whitelist_enable"
  "Enable or disable the approval whitelist"
  :input-schema (list :enable "boolean")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "whitelist_enable" (function cl-tron-mcp/security:whitelist-enable))

   (register-tool
    "whitelist_status"
    "Get current whitelist status"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "whitelist_status" (function cl-tron-mcp/security:whitelist-status))

  ;;; Swank integration tools
  ;;; Connect CL-TRON-MCP to a running SBCL with Swank loaded
  ;;;
  ;;; IMPORTANT: These tools require a running Swank server.
  ;;; To start Swank in your SBCL session:
  ;;;   (ql:quickload :swank)
  ;;;   (swank:create-server :port 4006 :dont-close t)
  ;;;
  ;;; Then use swank_connect to connect. All state persists in the SBCL session.

    (register-tool
     "swank_connect"
     "Connect to a running SBCL with Swank loaded. PREREQUISITE: Start Swank in SBCL first with (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t). Default port is 4006. Returns connection status."
     :input-schema (list :host "string" :port "integer")
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_connect" (function cl-tron-mcp/swank:swank-connect))

    (register-tool
     "swank_disconnect"
     "Disconnect from the Swank server. The SBCL session continues running - only the MCP connection is closed."
     :input-schema nil
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_disconnect" (function cl-tron-mcp/swank:swank-disconnect))

    (register-tool
     "swank_status"
     "Get the current Swank connection status. Shows if connected, reader thread status, and event processor status. Use to verify connection before using other Swank tools."
     :input-schema nil
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_status" (function cl-tron-mcp/swank:swank-status))

    (register-tool
     "swank_eval"
     "Evaluate Lisp code in the connected SBCL session. REQUIRES: swank_connect first. Code runs in a persistent session - state is preserved across calls. Use for testing, debugging, and hot-patching code."
     :input-schema (list :code "string" :package "string")
     :output-schema (list :type "object")
     :requires-approval t)
    (register-tool-handler "swank_eval" (function cl-tron-mcp/swank:mcp-swank-eval))

    (register-tool
     "swank_compile"
     "Compile and load Lisp code in the connected SBCL. REQUIRES: swank_connect first. Use for hot-reloading function definitions. The filename parameter helps with debugging messages."
     :input-schema (list :code "string" :package "string" :filename "string")
     :output-schema (list :type "object")
     :requires-approval t)
    (register-tool-handler "swank_compile" (function cl-tron-mcp/swank:mcp-swank-compile))

    (register-tool
     "swank_threads"
     "List all threads in the connected SBCL. REQUIRES: swank_connect first. Shows thread names, status, and IDs for use with other thread tools."
     :input-schema nil
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_threads" (function cl-tron-mcp/swank:mcp-swank-threads))

    (register-tool
     "swank_abort"
     "Abort a specific thread in the connected SBCL. REQUIRES: swank_connect first. Useful when a thread is stuck or in an error condition."
     :input-schema (list :threadId "string")
     :output-schema (list :type "object")
     :requires-approval t)
    (register-tool-handler "swank_abort" (function cl-tron-mcp/swank:mcp-swank-abort))

    (register-tool
     "swank_interrupt"
     "Interrupt the current thread's execution in the connected SBCL. REQUIRES: swank_connect first. Triggers the debugger for long-running computations."
     :input-schema nil
     :output-schema (list :type "object")
     :requires-approval t)
    (register-tool-handler "swank_interrupt" (function cl-tron-mcp/swank:mcp-swank-interrupt))

    (register-tool
     "swank_backtrace"
     "Get the current call stack from the connected SBCL. REQUIRES: swank_connect first. Use after swank_eval triggers an error to see where it occurred. Shows function names and source locations."
     :input-schema nil
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_backtrace" (function cl-tron-mcp/swank:mcp-swank-backtrace))

    (register-tool
     "swank_inspect"
     "Inspect an object in the connected SBCL. REQUIRES: swank_connect first. EXPRESSION is evaluated, so use quoted symbols for variables: '*my-var*' or expressions: '(make-hash-table)'."
     :input-schema (list :expression "string")
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_inspect" (function cl-tron-mcp/swank:mcp-swank-inspect))

    (register-tool
     "swank_describe"
     "Describe a symbol in the connected SBCL. REQUIRES: swank_connect first. Shows documentation, argument list, and source location."
     :input-schema (list :expression "string")
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_describe" (function cl-tron-mcp/swank:mcp-swank-describe))

    (register-tool
     "swank_autodoc"
     "Get argument list and documentation for a symbol. REQUIRES: swank_connect first. Example: 'mapcar' shows (function list &rest more-lists)."
     :input-schema (list :symbol "string")
     :output-schema (list :type "object")
     :requires-approval nil)
    (register-tool-handler "swank_autodoc" (function cl-tron-mcp/swank:mcp-swank-autodoc))

    (register-tool
     "swank_completions"
     "Get symbol completions in the connected SBCL. REQUIRES: swank_connect first. Example: prefix 'mak' returns (make-array make-hash-table make-instance ...)."
     :input-schema (list :symbol "string" :package "string")
     :output-schema (list :type "object")
     :requires-approval nil)
     (register-tool-handler "swank_completions" (function cl-tron-mcp/swank:mcp-swank-completions))

   (register-tool
    "swank_get_restarts"
    "Get available restarts when in the debugger. REQUIRES: swank_connect first. Use after swank_eval triggers an error. Shows recovery options like ABORT, RETRY, USE-VALUE, CONTINUE."
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_get_restarts" (function cl-tron-mcp/swank:swank-get-restarts))

   (register-tool
    "swank_invoke_restart"
    "Invoke a restart by index to recover from an error. REQUIRES: swank_connect first. Use swank_get_restarts to see available options. Index is 1-based."
    :input-schema (list :restart_index "integer")
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_invoke_restart" (function cl-tron-mcp/swank:swank-invoke-restart))

   (register-tool
    "swank_continue"
    "Continue execution from the debugger. REQUIRES: swank_connect first. Resumes normal execution after an error (if the condition is continuable)."
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_continue" (function cl-tron-mcp/swank:swank-continue))

   (register-tool
    "swank_step"
    "Step into the next expression in the debugger. REQUIRES: swank_connect first. Use when in a stepping context (entered via (step ...)). Shows the next form to be evaluated."
    :input-schema (list :frame "integer")
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_step" (function cl-tron-mcp/swank:swank-step))

   (register-tool
    "swank_next"
    "Step over the next expression in the debugger. REQUIRES: swank_connect first. Evaluates the next form without stepping into function calls."
    :input-schema (list :frame "integer")
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_next" (function cl-tron-mcp/swank:swank-next))

   (register-tool
    "swank_out"
    "Step out of the current frame in the debugger. REQUIRES: swank_connect first. Finishes the current function and stops at the return point."
    :input-schema (list :frame "integer")
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_out" (function cl-tron-mcp/swank:swank-out))

   (register-tool
    "swank_debugger_state"
    "Get the current debugger state: which thread is in the debugger, the debugger level, and whether debugging is active. REQUIRES: swank_connect first."
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_debugger_state" (function cl-tron-mcp/swank:swank-debugger-state))

;;; Unified REPL tools (Swank)
;;;
;;; These tools work with Swank (Slime/Portacle/Sly).
;;;
;;; IMPORTANT: You must connect before using these tools.
;;;   Start Swank: (ql:quickload :swank) (swank:create-server :port 4006)

  (register-tool
   "repl_connect"
   "Connect to Swank REPL. PREREQUISITE: Start Swank in SBCL first: (ql:quickload :swank) (swank:create-server :port 4006). Default port 4006."
   :input-schema (list :type "string" :host "string" :port "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_connect" (function cl-tron-mcp/unified:repl-connect))

  (register-tool
   "repl_disconnect"
   "Disconnect from the current REPL. The Lisp session continues running - only the MCP connection is closed."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_disconnect" (function cl-tron-mcp/unified:repl-disconnect))

  (register-tool
   "repl_status"
   "Check REPL connection status and type. Shows :connected, :type (:swank), :host, and :port. Use to verify connection before using other REPL tools."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_status" (function cl-tron-mcp/unified:repl-status))

  (register-tool
   "repl_eval"
   "Evaluate Lisp code in the connected REPL session. REQUIRES: repl_connect first. Code runs in a persistent session - all state (variables, functions, packages) is preserved across calls."
   :input-schema (list :code "string" :package "string")
   :output-schema (list :type "object")
   :requires-approval t)
  (register-tool-handler "repl_eval" (function cl-tron-mcp/unified:repl-eval))

  (register-tool
   "repl_compile"
   "Compile and load Lisp code in the connected REPL. REQUIRES: repl_connect first. Use for hot-reloading function definitions without restarting the session."
   :input-schema (list :code "string" :package "string" :filename "string")
   :output-schema (list :type "object")
   :requires-approval t)
  (register-tool-handler "repl_compile" (function cl-tron-mcp/unified:repl-compile))

  (register-tool
   "repl_threads"
   "List all threads in the connected REPL. REQUIRES: repl_connect first. Shows thread names, status, and IDs for debugging concurrency issues."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_threads" (function cl-tron-mcp/unified:repl-threads))

  (register-tool
   "repl_abort"
   "Abort/interrupt evaluation in the connected REPL. REQUIRES: repl_connect first. Use when a computation is stuck or taking too long."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_abort" (function cl-tron-mcp/unified:repl-abort))

  (register-tool
   "repl_backtrace"
   "Get the call stack from the connected REPL. REQUIRES: repl_connect first. Use after an error to see where it occurred in your code."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_backtrace" (function cl-tron-mcp/unified:repl-backtrace))

  (register-tool
   "repl_inspect"
   "Inspect an object in the connected REPL. REQUIRES: repl_connect first. EXPRESSION is evaluated - use quoted symbols for variables."
   :input-schema (list :expression "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_inspect" (function cl-tron-mcp/unified:repl-inspect))

  (register-tool
   "repl_describe"
   "Describe a symbol in the connected REPL. REQUIRES: repl_connect first. Shows documentation, argument list, and type information."
   :input-schema (list :symbol "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_describe" (function cl-tron-mcp/unified:repl-describe))

  (register-tool
   "repl_completions"
   "Get symbol completions in the connected REPL. REQUIRES: repl_connect first. Useful for discovering available functions and variables."
   :input-schema (list :prefix "string" :package "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_completions" (function cl-tron-mcp/unified:repl-completions))

   (register-tool
    "repl_doc"
    "Get documentation for a symbol in the connected REPL. REQUIRES: repl_connect first. Returns docstring and argument list."
    :input-schema (list :symbol "string")
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "repl_doc" (function cl-tron-mcp/unified:repl-doc))

;;; Unified REPL debugger tools
;;; These tools work with the debugger in Swank connections.

  (register-tool
   "repl_frame_locals"
   "Get local variables for a stack frame. REQUIRES: repl_connect and an active debugger session. Use after an error to inspect variable values at the point of failure."
   :input-schema (list :frame "integer" :thread "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_frame_locals" (function cl-tron-mcp/unified:repl-frame-locals))

  (register-tool
   "repl_step"
   "Step into the next expression. REQUIRES: repl_connect and an active stepping context (entered via (step ...)). Shows the next form to be evaluated."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_step" (function cl-tron-mcp/unified:repl-step))

  (register-tool
   "repl_next"
   "Step over the next expression. REQUIRES: repl_connect and an active stepping context. Evaluates the next form without stepping into function calls."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_next" (function cl-tron-mcp/unified:repl-next))

  (register-tool
   "repl_out"
   "Step out of the current frame. REQUIRES: repl_connect and an active stepping context. Finishes the current function and stops at the return point."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_out" (function cl-tron-mcp/unified:repl-out))

  (register-tool
   "repl_continue"
   "Continue execution from the debugger. REQUIRES: repl_connect and an active debugger session. Resumes normal execution."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_continue" (function cl-tron-mcp/unified:repl-continue))

  (register-tool
   "repl_get_restarts"
   "Get available restarts for error recovery. REQUIRES: repl_connect and an active debugger session. Shows options like ABORT, RETRY, USE-VALUE."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_get_restarts" (function cl-tron-mcp/unified:repl-get-restarts))

  (register-tool
   "repl_invoke_restart"
   "Invoke a restart by index to recover from an error. REQUIRES: repl_connect and an active debugger session. Use repl_get_restarts to see available options. Index is 1-based."
   :input-schema (list :restartIndex "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_invoke_restart" (function cl-tron-mcp/unified:repl-invoke-restart))

;;; Unified REPL breakpoint tools
  (register-tool
   "repl_set_breakpoint"
   "Set a breakpoint on a function via the connected REPL"
   :input-schema (list :function "string" :condition "string" :hitCount "integer" :thread "string")
   :output-schema (list :type "object")
   :requires-approval t)
  (register-tool-handler "repl_set_breakpoint" (function cl-tron-mcp/unified:repl-set-breakpoint))

  (register-tool
   "repl_remove_breakpoint"
   "Remove a breakpoint by ID via the connected REPL"
   :input-schema (list :breakpointId "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_remove_breakpoint" (function cl-tron-mcp/unified:repl-remove-breakpoint))

  (register-tool
   "repl_list_breakpoints"
   "List all breakpoints via the connected REPL"
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_list_breakpoints" (function cl-tron-mcp/unified:repl-list-breakpoints))

  (register-tool
   "repl_toggle_breakpoint"
   "Toggle breakpoint enabled state via the connected REPL"
   :input-schema (list :breakpointId "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_toggle_breakpoint" (function cl-tron-mcp/unified:repl-toggle-breakpoint))

;;; Unified REPL help tool
  (register-tool
   "repl_help"
   "Get help on available unified REPL tools"
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_help" (function cl-tron-mcp/unified:repl-help))
