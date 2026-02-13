;;;; test-http-simple.lisp - Ultra-simple HTTP test

(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)
(asdf:load-system :cl-tron-mcp)

(format t "Starting ultra-simple HTTP server on port 999...9~%")
(force-output)

(cl-tron-mcp/transport:http-server-loop 9999)
