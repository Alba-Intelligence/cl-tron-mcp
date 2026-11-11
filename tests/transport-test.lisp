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
             (ok (= cl-tron-mcp/transport:*rate-limit-requests-per-minute* 600))
             (ok (= cl-tron-mcp/transport:*max-request-size* (* 10 1024 1024)))
             (ok (= cl-tron-mcp/transport:*http-connection-timeout* 10))))

(deftest http-header-host-part-test
  (testing "Host/Origin parsing extracts the bare hostname"
    (ok (string= (cl-tron-mcp/transport::header-host-part "http://localhost:4006") "localhost"))
    (ok (string= (cl-tron-mcp/transport::header-host-part "127.0.0.1:4006") "127.0.0.1"))
    (ok (string= (cl-tron-mcp/transport::header-host-part "http://[::1]:4006/x") "[::1]"))
    (ok (string= (cl-tron-mcp/transport::header-host-part "https://evil.example") "evil.example"))
    (ok (not (member (cl-tron-mcp/transport::header-host-part "http://localhost.evil.example")
                     cl-tron-mcp/transport::*allowed-http-hosts* :test #'string=)))))

(deftest lisp-eval-endpoint-removed-test
  (testing "The unauthenticated eval handler no longer exists"
    (ok (not (fboundp (find-symbol "LISP-EVAL-HANDLER" :cl-tron-mcp/transport))))))

;;; stdio error replies (P3)

(defun stdio-roundtrip (line &optional (handler #'cl-tron-mcp/protocol:handle-message))
  "Feed LINE through the stdio line processor; return the list of parsed replies."
  (let ((replies '()))
    (cl-tron-mcp/transport::process-stdio-line
     line handler (lambda (r) (push (cl-tron-mcp/json-compat:parse
                                      (if (stringp r) r (cl-tron-mcp/json-compat:to-json r)))
                                    replies)))
    (nreverse replies)))

(deftest stdio-parse-error-gets-a-reply-test
  (testing "A line that is not JSON yields a -32700 error instead of silence"
    (let ((replies (stdio-roundtrip "this is not json")))
      (ok (= 1 (length replies)))
      (ok (= -32700 (getf (getf (first replies) :|error|) :|code|))))))

(deftest stdio-handler-failure-gets-a-reply-with-id-test
  (testing "If the handler fails, the client waiting on that id gets an error with the same id"
    (let ((replies (stdio-roundtrip "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"x\"}"
                                    (lambda (m) (declare (ignore m)) (error "kaboom")))))
      (ok (= 1 (length replies)))
      (ok (= 5 (getf (first replies) :|id|)))
      (ok (= -32603 (getf (getf (first replies) :|error|) :|code|))))))

(deftest stdio-blank-lines-and-notifications-get-no-reply-test
  (testing "Blank lines and notifications produce no output"
    (ok (null (stdio-roundtrip "")))
    (ok (null (stdio-roundtrip "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))))

;;; HTTP status / error mapping (P4)

(deftest http-parse-failure-is-jsonrpc-error-test
  (multiple-value-bind (body status) (cl-tron-mcp/transport::handle-rpc-body "{nope")
    (ok (= 400 status))
    (ok (= -32700 (getf (getf (cl-tron-mcp/json-compat:parse body) :|error|) :|code|)))))

(deftest http-notification-is-202-without-body-test
  (multiple-value-bind (body status)
      (cl-tron-mcp/transport::handle-rpc-body
       "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
    (ok (= 202 status))
    (ok (equal "" body))))

(deftest http-request-gets-200-test
  (multiple-value-bind (body status)
      (cl-tron-mcp/transport::handle-rpc-body "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}")
    (ok (= 200 status))
    (ok (= 3 (getf (cl-tron-mcp/json-compat:parse body) :|id|)))))

;;; Rate limiter (P2)

(deftest rate-limit-prunes-stale-entries-test
  (testing "Entries older than the window are dropped so the table cannot grow without bound"
    (clrhash cl-tron-mcp/transport::*request-counts*)
    (let ((now (get-universal-time)))
      (dotimes (i 50)
        (setf (gethash (format nil "10.0.0.~d" i) cl-tron-mcp/transport::*request-counts*)
              (list :count 1 :last-reset (- now 1000))))
      (cl-tron-mcp/transport::check-rate-limit "127.0.0.1")
      (ok (<= (hash-table-count cl-tron-mcp/transport::*request-counts*) 1)))
    (clrhash cl-tron-mcp/transport::*request-counts*)))

(deftest rate-limit-default-allows-an-active-agent-test
  (testing "A busy single client (loopback) is not throttled at the old 60/min"
    (clrhash cl-tron-mcp/transport::*request-counts*)
    (ok (loop repeat 200 always (cl-tron-mcp/transport::check-rate-limit "127.0.0.1")))
    (clrhash cl-tron-mcp/transport::*request-counts*)))
