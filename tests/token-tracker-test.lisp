;;;; tests/token-tracker-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest count-tokens-basic
  (testing "Basic token counting"
    (ok (= 1 (cl-tron-mcp/core:count-tokens "a")))
    (ok (= 1 (cl-tron-mcp/core:count-tokens "abc")))
    (ok (= 1 (cl-tron-mcp/core:count-tokens "abcd")))
    (ok (= 2 (cl-tron-mcp/core:count-tokens "abcdefgh")))
    (ok (= 3 (cl-tron-mcp/core:count-tokens "abcdefghijkl")))
    (ok (= 25 (cl-tron-mcp/core:count-tokens (make-string 100 :initial-element #\x))))))

(deftest count-tokens-empty
  (testing "Empty string token counting"
    (ok (= 1 (cl-tron-mcp/core:count-tokens "")))))

(deftest count-response-tokens-string
  (testing "Token counting for string responses"
    (ok (= 2 (cl-tron-mcp/core:count-response-tokens "abcdefgh")))
    (ok (= 9 (cl-tron-mcp/core:count-response-tokens "This is a test string with some text")))))

(deftest count-response-tokens-hash-table
  (testing "Token counting for hash table responses"
    (let ((response (make-hash-table)))
      (setf (gethash "message" response) "Hello world")
      (setf (gethash "status" response) "OK")
      (ok (= 8 (cl-tron-mcp/core:count-response-tokens response))))))

(deftest count-response-tokens-list
  (testing "Token counting for list responses"
    (let ((response '("item1" "item2")))
      (ok (= 4 (cl-tron-mcp/core:count-response-tokens response))))))

(deftest count-response-tokens-number
  (testing "Token counting for numeric responses"
    (ok (= 1 (cl-tron-mcp/core:count-response-tokens 42)))
    (ok (= 1 (cl-tron-mcp/core:count-response-tokens 3.14159)))))

(deftest count-response-tokens-boolean
  (testing "Token counting for boolean responses"
    (ok (= 1 (cl-tron-mcp/core:count-response-tokens t)))
    (ok (= 1 (cl-tron-mcp/core:count-response-tokens nil)))))

(deftest track-response-basic
  (testing "Basic response tracking"
    (cl-tron-mcp/core:reset-token-stats)
    (cl-tron-mcp/core:track-response "test-tool" "Hello world")
    (let ((stats (cl-tron-mcp/core:get-token-stats)))
      (ok (gethash "test-tool" stats))
      (destructuring-bind (tokens calls) (gethash "test-tool" stats)
        (ok (= 3 tokens))
        (ok (= 1 calls))))))

(deftest track-response-multiple
  (testing "Multiple response tracking"
    (cl-tron-mcp/core:reset-token-stats)
    (cl-tron-mcp/core:track-response "tool-a" "Response A")
    (cl-tron-mcp/core:track-response "tool-a" "Response A again")
    (cl-tron-mcp/core:track-response "tool-b" "Response B")
    (let ((stats (cl-tron-mcp/core:get-token-stats)))
      (destructuring-bind (tokens calls) (gethash "tool-a" stats)
        (ok (= 7 tokens))
        (ok (= 2 calls)))
      (destructuring-bind (tokens calls) (gethash "tool-b" stats)
        (ok (= 3 tokens))
        (ok (= 1 calls))))))

(deftest track-response-disabled
  (testing "Tracking when disabled"
    (cl-tron-mcp/core:reset-token-stats)
    (let ((cl-tron-mcp/core:*token-tracking-enabled* nil))
      (cl-tron-mcp/core:track-response "test-tool" "Hello world")
      (let ((stats (cl-tron-mcp/core:get-token-stats)))
        (ok (not (gethash "test-tool" stats)))))))

(deftest get-token-stats
  (testing "Getting token statistics"
    (cl-tron-mcp/core:reset-token-stats)
    (cl-tron-mcp/core:track-response "tool-1" "Response 1")
    (cl-tron-mcp/core:track-response "tool-2" "Response 2")
    (let ((stats (cl-tron-mcp/core:get-token-stats)))
      (ok (hash-table-p stats))
      (ok (= 2 (hash-table-count stats)))
      (ok (gethash "tool-1" stats))
      (ok (gethash "tool-2" stats)))))

(deftest reset-token-stats
  (testing "Resetting token statistics"
    (cl-tron-mcp/core:reset-token-stats)
    (cl-tron-mcp/core:track-response "tool-1" "Response 1")
    (cl-tron-mcp/core:track-response "tool-2" "Response 2")
    (ok (= 2 (hash-table-count (cl-tron-mcp/core:get-token-stats))))
    (cl-tron-mcp/core:reset-token-stats)
    (ok (= 0 (hash-table-count (cl-tron-mcp/core:get-token-stats))))))

(deftest generate-token-report
  (testing "Generating token report"
    (cl-tron-mcp/core:reset-token-stats)
    (cl-tron-mcp/core:track-response "tool-a" "Response A")
    (cl-tron-mcp/core:track-response "tool-a" "Response A again")
    (cl-tron-mcp/core:track-response "tool-b" "Response B")
    (let ((report (cl-tron-mcp/core:generate-token-report)))
      (ok (hash-table-p report))
      (ok (gethash "total-tokens" report))
      (ok (gethash "total-calls" report))
      (ok (gethash "average-tokens-per-call" report))
      (ok (gethash "tokens-per-tool" report))
      (ok (gethash "top-10-tools" report))
      (ok (= 10 (gethash "total-tokens" report)))
      (ok (= 3 (gethash "total-calls" report)))
      (ok (floatp (gethash "average-tokens-per-call" report))))))

(deftest generate-token-report-empty
  (testing "Generating token report with no data"
    (cl-tron-mcp/core:reset-token-stats)
    (let ((report (cl-tron-mcp/core:generate-token-report)))
      (ok (hash-table-p report))
      (ok (= 0 (gethash "total-tokens" report)))
      (ok (= 0 (gethash "total-calls" report)))
      (ok (= 0 (gethash "average-tokens-per-call" report))))))

(deftest with-token-tracking
  (testing "Token tracking macro"
    (cl-tron-mcp/core:reset-token-stats)
    (multiple-value-bind (result report)
        (cl-tron-mcp/core:with-token-tracking
          (cl-tron-mcp/core:track-response "tool-1" "Response 1")
          (cl-tron-mcp/core:track-response "tool-2" "Response 2")
          "done")
      (ok (stringp result))
      (ok (hash-table-p report))
      (ok (gethash "tokens-used" report)))))

(deftest benchmark-tool-simple
  (testing "Simple tool benchmarking"
    (cl-tron-mcp/core:reset-token-stats)
    (let* ((test-fn (lambda (x) (format nil "Result: ~a" x)))
           (result (cl-tron-mcp/core:benchmark-tool test-fn 42)))
      (ok (stringp result))
      (ok (search "Result: 42" result)))))