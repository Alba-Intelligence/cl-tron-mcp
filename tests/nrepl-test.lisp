;;;; tests/nrepl-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest nrepl-connection-state-test
  (testing "nrepl connection state variables exist"
    (ok (boundp 'cl-tron-mcp/nrepl::*nrepl-connected*))
    (ok (boundp 'cl-tron-mcp/nrepl::*nrepl-server*))
    (ok (boundp 'cl-tron-mcp/nrepl::*nrepl-stream*))))

(deftest nrepl-error-handling-test
  (testing "nrepl functions return errors when not connected"
    (let ((result (cl-tron-mcp/nrepl:nrepl-eval "(+ 1 2)")))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/nrepl:nrepl-threads)))
      (ok (getf result :error)))
    (let ((result (cl-tron-mcp/nrepl:nrepl-completions "mak")))
      (ok (getf result :error)))))
