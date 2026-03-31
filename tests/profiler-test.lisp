;;;; tests/profiler-test.lisp - Unit tests for profiler tools

(in-package :cl-tron-mcp/tests)

(deftest profiler-start-stop-test
  (testing "profile-start returns success"
    (let ((result (cl-tron-mcp/profiler:profile-start)))
      (ok (listp result))
      (ok (or (getf result :success) (getf result :started)))))
  (testing "profile-stop returns success after start"
    (let ((result (cl-tron-mcp/profiler:profile-stop)))
      (ok (listp result))
      (ok (or (getf result :success) (getf result :stopped))))))

(deftest profiler-report-test
  (testing "profile-report returns report data"
    (cl-tron-mcp/profiler:profile-start)
    ;; Do some work to profile
    (let ((x (loop for i from 1 to 1000 sum i)))
      (declare (ignore x)))
    (cl-tron-mcp/profiler:profile-stop)
    (let ((result (cl-tron-mcp/profiler:profile-report)))
      (ok (listp result)))))

(deftest profiler-reset-test
  (testing "profile-reset succeeds"
    (let ((result (cl-tron-mcp/profiler:profile-reset)))
      (ok (listp result)))))
