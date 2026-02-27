;;;; src/tools/tracer-tools.lisp
;;;; Tracer tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "trace_function"
  "Trace a function"
  :input-schema (list :functionName "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/trace-function.md"
  :validation ((validate-symbol-name "function_name" function_name :required t))
  :body (cl-tron-mcp/tracer:trace-function :function-name function_name))

(define-validated-tool "trace_remove"
  "Remove function trace"
  :input-schema (list :functionName "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/trace-remove.md"
  :validation ((validate-symbol-name "function_name" function_name :required t))
  :body (cl-tron-mcp/tracer:trace-remove :function-name function_name))

(define-simple-tool "trace_list"
  "List traced functions"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/trace-list.md"
  :function cl-tron-mcp/tracer:trace-list)