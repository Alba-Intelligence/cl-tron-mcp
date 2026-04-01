;;;; tests/logging-test.lisp - Unit tests for logging tools

(in-package :cl-tron-mcp/tests)

(deftest log-configure-test
    (testing "log-configure accepts valid log levels"
             (let ((result (cl-tron-mcp/logging:log-configure :level :info)))
               (ok (listp result))))
  (testing "log-configure with package param succeeds"
           (let ((result (cl-tron-mcp/logging:log-configure :level :debug :package "cl-tron-mcp/core")))
             (ok (listp result)))))

(deftest log-info-test
    (testing "log-info writes without error"
             (let ((result (cl-tron-mcp/logging:log-info "test info message")))
               (ok (listp result)))))

(deftest log-debug-test
    (testing "log-debug writes without error"
             (let ((result (cl-tron-mcp/logging:log-debug "test debug message")))
               (ok (listp result)))))

(deftest log-warn-test
    (testing "log-warn writes without error"
             (let ((result (cl-tron-mcp/logging:log-warn "test warn message")))
               (ok (listp result)))))

(deftest log-error-test
    (testing "log-error writes without error"
             (let ((result (cl-tron-mcp/logging:log-error "test error message")))
               (ok (listp result)))))

(deftest log-level-test
    (testing "log-level returns current level"
             (let ((result (cl-tron-mcp/logging:log-level)))
               ;; log-level returns a keyword or string
               (ok (not (null result))))))
