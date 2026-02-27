;;;; src/tools/tracer-tools.lisp
;;;; Tracer tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "trace_function"
  "Add trace to a function. Every call will print arguments and return value. Use to understand execution flow. Requires approval."
  :input-schema (list :functionName "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-symbol-name "function_name" function_name :required t))
  :body (cl-tron-mcp/tracer:trace-function :function-name function_name))

(define-validated-tool "trace_remove"
  "Remove trace from a function. Use when done debugging to stop trace output."
  :input-schema (list :functionName "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-symbol-name "function_name" function_name :required t))
  :body (cl-tron-mcp/tracer:trace-remove :function-name function_name))

(define-simple-tool "trace_list"
  "List all currently traced functions. Use to see what traces are active."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/tracer:trace-list)