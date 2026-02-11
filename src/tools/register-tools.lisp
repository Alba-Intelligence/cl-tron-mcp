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
