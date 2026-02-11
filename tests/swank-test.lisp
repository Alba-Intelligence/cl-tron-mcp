;;;; tests/swank-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest swank-help-test
  (testing "Swank help function returns tool list"
    (let ((result (cl-tron-mcp/swank:swank-help)))
      (ok (listp result))
      (ok (getf result :tools))
      (ok (> (length (getf result :tools)) 10)))))

(deftest swank-connection-state-test
  (testing "Swank connection state variables exist"
    (ok (boundp 'cl-tron-mcp/swank::*swank-connected*))
    (ok (boundp 'cl-tron-mcp/swank::*swank-server*))
    (ok (boundp 'cl-tron-mcp/swank::*swank-io*))))

(deftest swank-error-handling-test
  (testing "Swank functions return errors when not connected"
    ;; These should return error responses
    (let ((result (cl-tron-mcp/swank:send-swank-message "test")))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/swank:mcp-swank-threads)))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/swank:mcp-swank-backtrace)))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/swank:mcp-swank-completions :symbol "mak")))
      (ok (getf result :error)))))
