;;;; test-lisply-endpoints.lisp - Test the lisply-mcp compatible HTTP endpoints

(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)

(format t "Loading cl-tron-mcp...~%")
(asdf:load-system :cl-tron-mcp)
(format t "Loaded!~%")
(force-output)

;; Start HTTP server on a test port
(format t "Starting HTTP server on port 9998...~%")
(force-output)

(cl-tron-mcp/transport:start-http-transport :port 9998)

;; Give the server time to start
(sleep 1)

(format t "~%Testing endpoints...~%~%")
(force-output)

;; Test 1: Ping endpoint
(format t "Test 1: GET /lisply/ping-lisp~%")
(force-output)
(let ((response (dex:get "http://127.0.0.1:9998/lisply/ping-lisp" :want-stream nil)))
  (format t "Response: ~a~%~%" response))

;; Test 2: Lisp eval endpoint
(format t "Test 2: POST /lisply/lisp-eval~%")
(force-output)
(let* ((payload "{\"code\": \"(+ 1 2 3)\"}")
       (response (dex:post "http://127.0.0.1:9998/lisply/lisp-eval"
                          :content payload
                          :headers '(("Content-Type" . "application/json")))))
  (format t "Response: ~a~%~%" response))

;; Test 3: Lisp eval with package
(format t "Test 3: POST /lisply/lisp-eval with package~%")
(force-output)
(let* ((payload "{\"code\": \"*package*\", \"package\": \"CL-USER\"}")
       (response (dex:post "http://127.0.0.1:9998/lisply/lisp-eval"
                          :content payload
                          :headers '(("Content-Type" . "application/json")))))
  (format t "Response: ~a~%~%" response))

;; Test 4: Error handling
(format t "Test 4: POST /lisply/lisp-eval with invalid code~%")
(force-output)
(let* ((payload "{\"code\": \"(this-is-invalid)\"}")
       (response (dex:post "http://127.0.0.1:9998/lisply/lisp-eval"
                          :content payload
                          :headers '(("Content-Type" . "application/json")))))
  (format t "Response: ~a~%~%" response))

;; Test 5: Tools list
(format t "Test 5: GET /lisply/tools/list~%")
(force-output)
(let ((response (dex:get "http://127.0.0.1:9998/lisply/tools/list" :want-stream nil)))
  (format t "Response: ~a~%~%" response))

(format t "All tests complete!~%")

;; Stop the server
(cl-tron-mcp/transport:stop-http-transport)
(format t "Server stopped.~%")
