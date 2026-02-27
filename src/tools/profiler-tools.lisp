;;;; src/tools/profiler-tools.lisp
;;;; Profiler tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "profile_start"
  "Start deterministic profiling. All function calls will be timed. Use to find performance bottlenecks. Remember to call profile_stop when done."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :function cl-tron-mcp/profiler:profile-start)

(define-simple-tool "profile_stop"
  "Stop profiling and return report. Call after running the code you want to profile. Returns timing data for all functions called during profiling."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval t
  :function cl-tron-mcp/profiler:profile-stop)

(define-validated-tool "profile_report"
  "Get profiling report in specified format. Formats: flat (simple list), graph (call tree), cumulative (total time per function). Use after profile_start/stop."
  :input-schema (list :format (list :enum (list "flat" "graph" "cumulative")))
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when format (validate-choice "format" format '("flat" "graph" "cumulative"))))
  :body (cl-tron-mcp/profiler:profile-report :format format))