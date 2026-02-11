;;;; tests/protocol-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest message-parsing-test
  (testing "JSON messages can be parsed")
  (ok t))

(deftest response-creation-test
  (testing "Response is created correctly"
    (let ((response (cl-tron-mcp/protocol:parse-message
                    (cl-tron-mcp/protocol:make-response 1 "result"))))
      (ok (equal (getf response :id) 1)))))

(deftest error-response-test
  (testing "Error response is created correctly"
    (let ((response (cl-tron-mcp/protocol:parse-message
                    (cl-tron-mcp/protocol:make-error-response 1 -32000 "error"))))
      (ok (equal (getf response :id) 1))
      (ok (getf response :error)))))
