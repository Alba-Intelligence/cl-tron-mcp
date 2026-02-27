;;;; src/tools/profiler-tools.lisp
;;;; Profiler tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "profile_start"
  "Start profiling"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/profile-start.md"
  :function cl-tron-mcp/profiler:profile-start)

(define-simple-tool "profile_stop"
  "Stop profiling"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/profile-stop.md"
  :function cl-tron-mcp/profiler:profile-stop)

(define-validated-tool "profile_report"
  "Get profiling report"
  :input-schema (list :format (list :enum (list "flat" "graph" "cumulative")))
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/profile-report.md"
  :validation ((when format (validate-choice "format" format '("flat" "graph" "cumulative"))))
  :body (cl-tron-mcp/profiler:profile-report :format format))