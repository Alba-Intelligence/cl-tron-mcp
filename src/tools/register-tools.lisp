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
 "Evaluate Lisp code in REPL context. Use for testing, debugging, and modifying running code. Requires connection to Swank/nrepl. Code runs in the persistent Lisp session."
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

   (register-tool
    "swank_connect"
    "Connect to a running SBCL with Swank loaded. On SBCL: (ql:quickload :swank) (swank:create-server :port 4005)"
    :input-schema (list :host "string" :port "integer")
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_connect" (function cl-tron-mcp/swank:swank-connect))

   (register-tool
    "swank_disconnect"
    "Disconnect from the Swank server"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_disconnect" (function cl-tron-mcp/swank:swank-disconnect))

   (register-tool
    "swank_status"
    "Get the current Swank connection status"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_status" (function cl-tron-mcp/swank:swank-status))

   (register-tool
    "swank_eval"
    "Evaluate Lisp code via Swank. Use this to execute code in the connected SBCL."
    :input-schema (list :code "string" :package "string")
    :output-schema (list :type "object")
    :requires-approval t)
   (register-tool-handler "swank_eval" (function cl-tron-mcp/swank:mcp-swank-eval))

   (register-tool
    "swank_compile"
    "Compile Lisp code via Swank"
    :input-schema (list :code "string" :package "string" :filename "string")
    :output-schema (list :type "object")
    :requires-approval t)
   (register-tool-handler "swank_compile" (function cl-tron-mcp/swank:mcp-swank-compile))

   (register-tool
    "swank_threads"
    "List all threads in the Swank-connected SBCL"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_threads" (function cl-tron-mcp/swank:mcp-swank-threads))

   (register-tool
    "swank_abort"
    "Abort a specific thread, or the current thread if not specified"
    :input-schema (list :threadId "string")
    :output-schema (list :type "object")
    :requires-approval t)
   (register-tool-handler "swank_abort" (function cl-tron-mcp/swank:mcp-swank-abort))

   (register-tool
    "swank_interrupt"
    "Interrupt the current thread's execution"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval t)
   (register-tool-handler "swank_interrupt" (function cl-tron-mcp/swank:mcp-swank-interrupt))

   (register-tool
    "swank_backtrace"
    "Get the current backtrace"
    :input-schema nil
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_backtrace" (function cl-tron-mcp/swank:mcp-swank-backtrace))

   (register-tool
    "swank_inspect"
    "Inspect an object via Swank. Example: \"*package*\" or \"(make-hash-table)\""
    :input-schema (list :expression "string")
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_inspect" (function cl-tron-mcp/swank:mcp-swank-inspect))

   (register-tool
    "swank_describe"
    "Describe an object via Swank. Example: \"car\" or \"list\""
    :input-schema (list :expression "string")
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_describe" (function cl-tron-mcp/swank:mcp-swank-describe))

   (register-tool
    "swank_autodoc"
    "Get documentation for a symbol. Example: \"mapcar\" or \"(car list)\""
    :input-schema (list :symbol "string")
    :output-schema (list :type "object")
    :requires-approval nil)
   (register-tool-handler "swank_autodoc" (function cl-tron-mcp/swank:mcp-swank-autodoc))

   (register-tool
    "swank_completions"
    "Get symbol completions via Swank. Example: \"mak\" returns (make-array make-hash-table ...)"
    :input-schema (list :symbol "string" :package "string")
    :output-schema (list :type "object")
    :requires-approval nil)
    (register-tool-handler "swank_completions" (function cl-tron-mcp/swank:mcp-swank-completions))

  (register-tool
   "swank_get_restarts"
   "Get available restarts when in debugger. Use after an error triggers the debugger."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_get_restarts" (function cl-tron-mcp/swank:swank-get-restarts))

  (register-tool
   "swank_invoke_restart"
   "Invoke a restart by index when in debugger. Use after swank_get_restarts to see available restarts."
   :input-schema (list :restart_index "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_invoke_restart" (function cl-tron-mcp/swank:swank-invoke-restart))

  (register-tool
   "swank_continue"
   "Continue execution from debugger."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_continue" (function cl-tron-mcp/swank:swank-continue))

  (register-tool
   "swank_step"
   "Step into next expression in debugger."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_step" (function cl-tron-mcp/swank:swank-step))

  (register-tool
   "swank_next"
   "Step over next expression in debugger."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_next" (function cl-tron-mcp/swank:swank-next))

  (register-tool
   "swank_out"
   "Step out of current frame in debugger."
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_out" (function cl-tron-mcp/swank:swank-out))

  (register-tool
   "swank_debugger_state"
   "Get current debugger state (thread, level, in-debugger-p)."
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
   (register-tool-handler "swank_debugger_state" (function cl-tron-mcp/swank:swank-debugger-state))

;;; nrepl tools (Sly, CIDER compatibility)
 (register-tool
  "nrepl_connect"
  "Connect to an nrepl server (Sly, CIDER)"
  :input-schema (list :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_connect" (function cl-tron-mcp/nrepl:nrepl-connect))

 (register-tool
  "nrepl_disconnect"
  "Disconnect from nrepl server"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_disconnect" (function cl-tron-mcp/nrepl:nrepl-disconnect))

 (register-tool
  "nrepl_status"
  "Check nrepl connection status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_status" (function cl-tron-mcp/nrepl:nrepl-status))

 (register-tool
  "nrepl_eval"
  "Evaluate Lisp code via nrepl"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t)
 (register-tool-handler "nrepl_eval" (function cl-tron-mcp/nrepl:nrepl-eval))

 (register-tool
  "nrepl_compile"
  "Compile Lisp code via nrepl"
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t)
 (register-tool-handler "nrepl_compile" (function cl-tron-mcp/nrepl:nrepl-compile))

 (register-tool
  "nrepl_sessions"
  "List all nrepl sessions"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_sessions" (function cl-tron-mcp/nrepl:nrepl-sessions))

 (register-tool
  "nrepl_close_session"
  "Close an nrepl session"
  :input-schema (list :session "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_close_session" (function cl-tron-mcp/nrepl:nrepl-close-session))

 (register-tool
  "nrepl_threads"
  "List all threads via nrepl"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_threads" (function cl-tron-mcp/nrepl:nrepl-threads))

 (register-tool
  "nrepl_interrupt"
  "Interrupt evaluation via nrepl"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_interrupt" (function cl-tron-mcp/nrepl:nrepl-interrupt))

 (register-tool
  "nrepl_backtrace"
  "Get backtrace via nrepl"
  :input-schema (list :thread "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_backtrace" (function cl-tron-mcp/nrepl:nrepl-backtrace))

 (register-tool
  "nrepl_inspect"
  "Inspect an object via nrepl"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_inspect" (function cl-tron-mcp/nrepl:nrepl-inspect))

 (register-tool
  "nrepl_describe"
  "Describe a symbol via nrepl"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_describe" (function cl-tron-mcp/nrepl:nrepl-describe))

 (register-tool
  "nrepl_doc"
  "Get documentation for a symbol via nrepl"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "nrepl_doc" (function cl-tron-mcp/nrepl:nrepl-doc))

 (register-tool
  "nrepl_completions"
  "Get symbol completions via nrepl"
  :input-schema (list :prefix "string")
  :output-schema (list :type "object")
  :requires-approval nil)
  (register-tool-handler "nrepl_completions" (function cl-tron-mcp/nrepl:nrepl-completions))

;;; Unified REPL tools (auto-detects Swank vs nrepl)
 (register-tool
  "repl_connect"
  "Connect to any Lisp REPL (auto-detects Swank/nrepl)"
  :input-schema (list :type "string" :host "string" :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_connect" (function cl-tron-mcp/unified:repl-connect))

 (register-tool
  "repl_disconnect"
  "Disconnect from the current REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_disconnect" (function cl-tron-mcp/unified:repl-disconnect))

 (register-tool
  "repl_status"
  "Check REPL connection status and type"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_status" (function cl-tron-mcp/unified:repl-status))

 (register-tool
  "repl_eval"
  "Evaluate Lisp code via the connected REPL"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t)
 (register-tool-handler "repl_eval" (function cl-tron-mcp/unified:repl-eval))

 (register-tool
  "repl_compile"
  "Compile Lisp code via the connected REPL"
  :input-schema (list :code "string" :package "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t)
 (register-tool-handler "repl_compile" (function cl-tron-mcp/unified:repl-compile))

 (register-tool
  "repl_threads"
  "List all threads via the connected REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_threads" (function cl-tron-mcp/unified:repl-threads))

 (register-tool
  "repl_abort"
  "Abort/interrupt evaluation via the connected REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_abort" (function cl-tron-mcp/unified:repl-abort))

 (register-tool
  "repl_backtrace"
  "Get backtrace via the connected REPL"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_backtrace" (function cl-tron-mcp/unified:repl-backtrace))

 (register-tool
  "repl_inspect"
  "Inspect an object via the connected REPL"
  :input-schema (list :expression "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_inspect" (function cl-tron-mcp/unified:repl-inspect))

 (register-tool
  "repl_describe"
  "Describe a symbol via the connected REPL"
  :input-schema (list :symbol "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_describe" (function cl-tron-mcp/unified:repl-describe))

 (register-tool
  "repl_completions"
  "Get symbol completions via the connected REPL"
  :input-schema (list :prefix "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "repl_completions" (function cl-tron-mcp/unified:repl-completions))

  (register-tool
   "repl_doc"
   "Get documentation for a symbol via the connected REPL"
   :input-schema (list :symbol "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_doc" (function cl-tron-mcp/unified:repl-doc))

;;; Unified REPL debugger tools
  (register-tool
   "repl_frame_locals"
   "Get local variables for a frame via the connected REPL"
   :input-schema (list :frame "integer" :thread "string")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_frame_locals" (function cl-tron-mcp/unified:repl-frame-locals))

  (register-tool
   "repl_step"
   "Step into next expression in frame via the connected REPL"
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_step" (function cl-tron-mcp/unified:repl-step))

  (register-tool
   "repl_next"
   "Step over next expression in frame via the connected REPL"
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_next" (function cl-tron-mcp/unified:repl-next))

  (register-tool
   "repl_out"
   "Step out of current frame via the connected REPL"
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_out" (function cl-tron-mcp/unified:repl-out))

  (register-tool
   "repl_continue"
   "Continue execution from debugger via the connected REPL"
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_continue" (function cl-tron-mcp/unified:repl-continue))

  (register-tool
   "repl_get_restarts"
   "Get available restarts via the connected REPL"
   :input-schema (list :frame "integer")
   :output-schema (list :type "object")
   :requires-approval nil)
  (register-tool-handler "repl_get_restarts" (function cl-tron-mcp/unified:repl-get-restarts))

  (register-tool
   "repl_invoke_restart"
   "Invoke a restart by index via the connected REPL"
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



