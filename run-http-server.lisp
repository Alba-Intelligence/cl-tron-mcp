;;;; run-http-server.lisp - Run HTTP server for testing

(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)
(asdf:load-system :cl-tron-mcp)

(format t "Starting HTTP server on port 3333...~%")
(force-output)

;; Run the server in a thread
(cl-tron-mcp/transport:start-http-transport :port 3333)

(format t "Server started. Running...~%")
(force-output)

;; Keep SBCL alive
(format t "Keeping SBCL alive...~%")
(force-output)
(loop (sleep 60))
