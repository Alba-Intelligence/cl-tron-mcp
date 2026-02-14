;;;; tests/swank-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest swank-connection-state-test
  (testing "Swank connection state variables exist"
    (ok (boundp 'cl-tron-mcp/swank::*swank-connected*))
    (ok (boundp 'cl-tron-mcp/swank::*swank-socket*))
    (ok (boundp 'cl-tron-mcp/swank::*swank-io*))))

(deftest swank-error-handling-test
  (testing "Swank functions return errors when not connected"
    ;; These should return error responses
    (let ((result (cl-tron-mcp/swank:mcp-swank-threads)))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/swank:mcp-swank-backtrace)))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/swank:mcp-swank-completions :prefix "mak")))
      (ok (getf result :error)))))
