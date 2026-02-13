;;;; test-http-server-foreground.lisp - Test HTTP server with curl commands

(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)

(format t "Loading cl-tron-mcp...~%")
(asdf:load-system :cl-tron-mcp)
(format t "Loaded!~%")
(force-output)

;; Run HTTP server in this thread (blocking)
(format t "Starting HTTP server on port 9997...~%")
(force-output)

(handler-case
    (cl-tron-mcp/transport:http-server-loop 9997)
  (error (e)
    (format t "Error: ~a~%" e)))

(format t "Server exited~%")
