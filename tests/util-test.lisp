;;;; tests/util-test.lisp - shared helpers (D1)

(in-package :cl-tron-mcp/tests)

(deftest get-unix-time-is-defined-once-test
  (testing "Every package that needs get-unix-time uses the one shared definition"
    (dolist (pkg '("CL-TRON-MCP/SECURITY" "CL-TRON-MCP/SWANK" "CL-TRON-MCP/MONITOR"
                   "CL-TRON-MCP/PROFILER" "CL-TRON-MCP/PROTOCOL"))
      (ok (eq (find-symbol "GET-UNIX-TIME" pkg) 'cl-tron-mcp/util:get-unix-time) pkg))))

(deftest get-unix-time-value-test
  (testing "It returns whole Unix seconds that agree with local-time"
    (let ((ours (cl-tron-mcp/util:get-unix-time))
          (theirs (local-time:timestamp-to-unix (local-time:now))))
      (ok (integerp ours))
      (ok (<= (abs (- ours theirs)) 2)))))
