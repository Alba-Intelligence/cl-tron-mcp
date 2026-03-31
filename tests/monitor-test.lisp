;;;; tests/monitor-test.lisp - Unit tests for monitor tools

(in-package :cl-tron-mcp/tests)

(deftest health-check-test
  (testing "health-check returns status plist"
    (let ((result (cl-tron-mcp/monitor:health-check)))
      (ok (listp result))
      (ok (getf result :status)))))

(deftest runtime-stats-test
  (testing "runtime-stats returns memory and thread info"
    (let ((result (cl-tron-mcp/monitor:runtime-stats)))
      (ok (listp result)))))

(deftest system-info-test
  (testing "system-info returns Lisp implementation info"
    (let ((result (cl-tron-mcp/monitor:system-info)))
      (ok (listp result)))))

(deftest gc-run-test
  (testing "gc-run with default generation succeeds"
    (let ((result (cl-tron-mcp/monitor:gc-run :generation 0)))
      (ok (listp result))
      (ok (getf result :success))))
  (testing "gc-run with invalid generation returns error"
    (let ((result (cl-tron-mcp/monitor:gc-run :generation 99)))
      (ok (listp result))
      (ok (getf result :error)))))
