;;;; tests/transport-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest http-response-test
  (testing "HTTP response is formatted correctly"
    (let ((response (cl-tron-mcp/transport:http-ok "Hello World" "application/json")))
      (ok (stringp response))
      (ok (search "200 OK" response))
      (ok (search "Content-Type: application/json" response))
      (ok (search "Content-Length:" response)))))

(deftest http-not-found-test
  (testing "HTTP 404 response is formatted correctly"
    (let ((response (cl-tron-mcp/transport:http-not-found)))
      (ok (stringp response))
      (ok (search "404 Not Found" response)))))

