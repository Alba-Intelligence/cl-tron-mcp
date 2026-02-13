(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)
(asdf:load-system :cl-tron-mcp)

(format t "[TEST] Starting read loop...~%")
(loop for i from 1 to 5
      do (format t "[TEST] Waiting for line ~d...~%" i)
         (let ((line (read-line *standard-input* nil)))
           (format t "[TEST] Got: ~a~%" line)
           (when (null line) (return))))
(format t "[TEST] Read loop done~%")
