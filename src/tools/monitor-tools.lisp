;;;; src/tools/monitor-tools.lisp
;;;; Monitor tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "health_check"
  "Server health check"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/health-check.md"
  :function cl-tron-mcp/monitor:health-check)

(define-simple-tool "runtime_stats"
  "Get runtime statistics"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/runtime-stats.md"
  :function cl-tron-mcp/monitor:runtime-stats)

(define-simple-tool "gc_run"
  "Force garbage collection"
  :input-schema (list :generation "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/gc-run.md"
  :function cl-tron-mcp/monitor:gc-run)

(define-simple-tool "system_info"
  "Get system information"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/system-info.md"
  :function cl-tron-mcp/monitor:system-info)