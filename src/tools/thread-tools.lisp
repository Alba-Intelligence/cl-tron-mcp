;;;; src/tools/thread-tools.lisp
;;;; Thread tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "thread_list"
  "List all threads with their status (running, waiting, etc). Use to monitor multi-threaded applications and debug concurrency issues."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/sbcl:list-threads)

(define-validated-tool "thread_inspect"
  "Get detailed information about a specific thread including name, state, and stack usage. Use to understand thread behavior."
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/sbcl:inspect-thread :thread-id thread_id))

(define-validated-tool "thread_backtrace"
  "Get backtrace for a specific thread. Use to see what a thread is currently doing or where it's blocked."
  :input-schema (list :threadId "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "thread_id" thread_id :required t))
  :body (cl-tron-mcp/sbcl:thread-backtrace :thread-id thread_id))