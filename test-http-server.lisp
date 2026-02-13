(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)

(format t "Loading cl-tron-mcp...~%")
(asdf:load-system :cl-tron-mcp)
(format t "Loaded!~%")
(force-output)

;; Run HTTP server directly in this thread
(format t "Starting HTTP server on port 9006...~%")
(force-output)

(handler-case
    (progn
      (cl-tron-mcp/transport:http-server-loop 9006))
  (error (e)
    (format t "Error: ~a~%" e)))

(format t "Server exited~%")
