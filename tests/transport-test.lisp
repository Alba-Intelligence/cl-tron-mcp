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

(deftest http-transport-config-test
  (testing "HTTP transport configuration variables are set"
    (ok (integerp cl-tron-mcp/transport:*max-concurrent-connections*))
    (ok (integerp cl-tron-mcp/transport:*http-request-timeout*))
    (ok (or (eq cl-tron-mcp/transport:*rate-limit-enabled* t)
            (eq cl-tron-mcp/transport:*rate-limit-enabled* nil)))
    (ok (integerp cl-tron-mcp/transport:*rate-limit-requests-per-minute*))
    (ok (integerp cl-tron-mcp/transport:*max-request-size*))
    (ok (integerp cl-tron-mcp/transport:*http-connection-timeout*))
    (ok (> cl-tron-mcp/transport:*max-concurrent-connections* 0))
    (ok (> cl-tron-mcp/transport:*http-request-timeout* 0))
    (ok (> cl-tron-mcp/transport:*rate-limit-requests-per-minute* 0))
    (ok (> cl-tron-mcp/transport:*max-request-size* 0))
    (ok (> cl-tron-mcp/transport:*http-connection-timeout* 0))))

(deftest http-transport-default-values-test
  (testing "HTTP transport has sensible default values"
    (ok (= cl-tron-mcp/transport:*max-concurrent-connections* 100))
    (ok (= cl-tron-mcp/transport:*http-request-timeout* 30))
    (ok cl-tron-mcp/transport:*rate-limit-enabled*)
    (ok (= cl-tron-mcp/transport:*rate-limit-requests-per-minute* 60))
    (ok (= cl-tron-mcp/transport:*max-request-size* (* 10 1024 1024)))
    (ok (= cl-tron-mcp/transport:*http-connection-timeout* 10))))

