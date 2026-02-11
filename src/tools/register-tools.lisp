;;;; src/tools/register-tools.lisp
;;;;
;;;; Tool registration module - loads after all tool modules are defined.
;;;; Registers all tools with the registry.

(in-package :cl-tron-mcp/tools)

;;; Inspector tools
(register-tool
 "inspect_object"
 "Inspect an object by ID"
 :input-schema (list :objectId "string" :maxDepth "integer")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_object" (function cl-tron-mcp/inspector:inspect-object))

(register-tool
 "inspect_slot"
 "Get or set a slot value on an object"
 :input-schema (list :objectId "string" :slotName "string" :value "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_slot" (function cl-tron-mcp/inspector:inspect-slot))

(register-tool
 "inspect_class"
 "Inspect a CLOS class definition"
 :input-schema (list :className "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_class" (function cl-tron-mcp/inspector:inspect-class))

(register-tool
 "inspect_function"
 "Inspect a function definition"
 :input-schema (list :symbolName "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_function" (function cl-tron-mcp/inspector:inspect-function))

(register-tool
 "inspect_package"
 "Inspect a package and list its contents"
 :input-schema (list :packageName "string")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "inspect_package" (function cl-tron-mcp/inspector:inspect-package))

;;; REPL tools
(register-tool
 "repl_eval"
 "Evaluate Lisp code in REPL context"
 :input-schema (list :code "string" :package "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "repl_eval" (function cl-tron-mcp/repl:repl-eval))

;;; Monitor tools
(register-tool
 "health_check"
 "Basic health check for the MCP server"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "health_check" (function cl-tron-mcp/monitor:health-check))

(register-tool
 "runtime_stats"
 "Get runtime statistics including memory and thread info"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "runtime_stats" (function cl-tron-mcp/monitor:runtime-stats))

;;; Debugger tools
(register-tool
 "debugger_frames"
 "Get debugger stack frames"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "debugger_frames" (function cl-tron-mcp/debugger:get-debugger-frames))

(register-tool
 "debugger_restarts"
  "List available debugger restarts"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil)
(register-tool-handler "debugger_restarts" (function cl-tron-mcp/debugger:list-restarts))

(register-tool
 "breakpoint_set"
 "Set a breakpoint on a function"
 :input-schema (list :functionName "string" :condition "string" :hitCount "integer")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "breakpoint_set" (function cl-tron-mcp/debugger:set-breakpoint))

(register-tool
 "breakpoint_remove"
 "Remove a breakpoint by ID"
 :input-schema (list :breakpointId "integer")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "breakpoint_remove" (function cl-tron-mcp/debugger:remove-breakpoint))

(register-tool
 "breakpoint_list"
 "List all active breakpoints"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "breakpoint_list" (function cl-tron-mcp/debugger:list-breakpoints))

(register-tool
 "step_frame"
  "Step execution in a frame"
  :input-schema (list :frame "integer" :mode "string")
  :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "step_frame" (function cl-tron-mcp/debugger:step-frame))

;;; Hot-reload tools
(register-tool
 "code_compile_string"
 "Compile and load Lisp code string into the image"
 :input-schema (list :code "string" :filename "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "code_compile_string" (function cl-tron-mcp/hot-reload:compile-and-load))

(register-tool
 "reload_system"
 "Reload ASDF system with dependencies"
 :input-schema (list :systemName "string" :force "boolean")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "reload_system" (function cl-tron-mcp/hot-reload:reload-system))

;;; Profiler tools
(register-tool
 "profile_start"
 "Start deterministic profiling"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "profile_start" (function cl-tron-mcp/profiler:profile-start))

(register-tool
 "profile_stop"
 "Stop profiling and return report"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "profile_stop" (function cl-tron-mcp/profiler:profile-stop))

;;; Tracer tools
(register-tool
 "trace_function"
 "Add trace to a function"
 :input-schema (list :functionName "string")
 :output-schema (list :type "object")
 :requires-approval t)
(register-tool-handler "trace_function" (function cl-tron-mcp/tracer:trace-function))

(register-tool
 "trace_list"
 "List all traced functions"
 :input-schema nil
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "trace_list" (function cl-tron-mcp/tracer:trace-list))

;;; SBCL tools
 (register-tool
  "thread_list"
   "List all threads with their status"
   :input-schema nil
   :output-schema (list :type "object")
   :requires-approval nil)
 (register-tool-handler "thread_list" (function cl-tron-mcp/sbcl:list-threads))

 (register-tool
  "thread_inspect"
  "Get detailed information about a thread"
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "thread_inspect" (function cl-tron-mcp/sbcl:inspect-thread))

 (register-tool
  "thread_backtrace"
  "Get backtrace for a specific thread"
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil)
 (register-tool-handler "thread_backtrace" (function cl-tron-mcp/sbcl:thread-backtrace))



