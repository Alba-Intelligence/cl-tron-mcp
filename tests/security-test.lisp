;;;; tests/security-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest approval-check-test
  (testing "Operation approval check"
    (ok (cl-tron-mcp/security:operation-requires-approval :eval))
    (ok (not (cl-tron-mcp/security:operation-requires-approval :unknown)))))

(deftest audit-log-test
  (testing "Audit log entries can be created"
    (cl-tron-mcp/security:log-operation :eval "test-tool" :user "test")
    (ok (> (length (cl-tron-mcp/security:get-audit-log)) 0))))
