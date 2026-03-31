;;;; src/tools/monitor-tools.lisp
;;;; Monitor tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "health_check"
  "Check MCP server and SBCL runtime health status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/health-check.md"
  :function cl-tron-mcp/monitor:health-check)

(define-simple-tool "runtime_stats"
  "Get memory usage, GC stats, and thread count"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/runtime-stats.md"
  :function cl-tron-mcp/monitor:runtime-stats)

(define-validated-tool "gc_run"
  "Force garbage collection (optional generation)"
  :input-schema (list :generation "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/gc-run.md"
  :validation ((when generation (validate-integer "generation" generation :min 0 :max 7)))
  :body (cl-tron-mcp/monitor:gc-run :generation (or generation 0)))

(define-simple-tool "system_info"
  "Get Lisp implementation, OS, and package count details"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/system-info.md"
  :function cl-tron-mcp/monitor:system-info)