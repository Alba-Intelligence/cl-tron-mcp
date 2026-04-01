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

(deftest audit-log-bounds-test
    (testing "Audit log respects size limit"
             (cl-tron-mcp/security:clear-audit-log)
             (let ((old-max cl-tron-mcp/security:*audit-log-max-size*))
               (setf cl-tron-mcp/security:*audit-log-max-size* 10)
               (unwind-protect
                    (progn
                      (dotimes (i 15)
                        (cl-tron-mcp/security:log-operation :eval "test-tool"))
                      (ok (<= (length (cl-tron-mcp/security:get-audit-log)) 10)))
                 (setf cl-tron-mcp/security:*audit-log-max-size* old-max)
                 (cl-tron-mcp/security:clear-audit-log)))))

(deftest audit-log-sanitize-test
    (testing "Sensitive parameters are redacted in audit log"
             (cl-tron-mcp/security:clear-audit-log)
             (let ((sanitized (cl-tron-mcp/security:sanitize-arguments
                               '(:password "s3cr3t" :code "(+ 1 2)"))))
               (ok (string= (getf sanitized :password) "[REDACTED]"))
               (ok (string= (getf sanitized :code) "(+ 1 2)")))))

(deftest whitelist-add-remove-test
    (testing "Whitelist entries can be added and removed"
             (cl-tron-mcp/security:whitelist-clear)
             (cl-tron-mcp/security:whitelist-enable t)
             (cl-tron-mcp/security:whitelist-add :eval t)
             (ok (cl-tron-mcp/security:whitelist-check :eval nil))
             (cl-tron-mcp/security:whitelist-remove :eval t)
             (cl-tron-mcp/security:whitelist-clear)))

(deftest whitelist-pattern-test
    (testing "Whitelist string pattern matching works"
             (cl-tron-mcp/security:whitelist-clear)
             (cl-tron-mcp/security:whitelist-enable t)
             (cl-tron-mcp/security:whitelist-add :eval "safe-prefix")
             (ok (cl-tron-mcp/security:whitelist-check :eval "safe-prefix-operation"))
             (ok (not (cl-tron-mcp/security:whitelist-check :eval "unsafe-operation")))
             (cl-tron-mcp/security:whitelist-clear)))

(deftest whitelist-wildcard-test
    (testing "Whitelist wildcard (t) matches everything"
             (cl-tron-mcp/security:whitelist-clear)
             (cl-tron-mcp/security:whitelist-enable t)
             (cl-tron-mcp/security:whitelist-add :eval t)
             (ok (cl-tron-mcp/security:whitelist-check :eval "anything"))
             (ok (cl-tron-mcp/security:whitelist-check :eval nil))
             (cl-tron-mcp/security:whitelist-clear)))

(deftest whitelist-disabled-test
    (testing "Whitelist returns NIL when disabled"
             (cl-tron-mcp/security:whitelist-enable nil)
             (cl-tron-mcp/security:whitelist-add :eval t)
             (ok (not (cl-tron-mcp/security:whitelist-check :eval nil)))
             (cl-tron-mcp/security:whitelist-clear)
             (cl-tron-mcp/security:whitelist-enable nil)))
