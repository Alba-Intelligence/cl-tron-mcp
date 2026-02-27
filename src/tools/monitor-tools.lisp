;;;; src/tools/monitor-tools.lisp
;;;; Monitor tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "health_check"
  "Basic health check for the MCP server. Use to verify Tron is running and responsive. Returns server status and version."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/monitor:health-check)

(define-simple-tool "runtime_stats"
  "Get runtime statistics including memory usage, thread count, and GC info. Use to monitor system health and diagnose memory issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/monitor:runtime-stats)

(define-simple-tool "gc_run"
  "Force garbage collection. Use to free memory or test GC behavior. Optionally specify generation (0-6 for SBCL)."
  :input-schema (list :generation "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/monitor:gc-run)

(define-simple-tool "system_info"
  "Get comprehensive system information including Lisp implementation, OS, and runtime details. Use for debugging environment issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/monitor:system-info)