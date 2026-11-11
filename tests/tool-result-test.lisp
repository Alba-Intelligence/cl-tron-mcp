;;;; tests/tool-result-test.lisp - tool result encoding (B7) and tool timeout (B4)

(in-package :cl-tron-mcp/tests)

(defun call-tool-via-protocol (name arguments &optional (id 1))
  "Run NAME through the protocol handler; return (values parsed-response result-text-json)."
  (let* ((response (cl-tron-mcp/protocol::handle-tool-call
                    id (list :|name| name :|arguments| arguments)))
         (parsed (cl-tron-mcp/json-compat:parse response))
         (content (getf (getf parsed :|result|) :|content|))
         (text (getf (first content) :|text|)))
    (values parsed text)))

(deftest tool-result-is-json-test
  (testing "A successful tool result is a JSON object with lowercase keys, not a printed plist"
    (multiple-value-bind (parsed text)
        (call-tool-via-protocol "skill_store_installation_guide"
                                (list :|target_platform| "claude" :|format| "json"))
      (let ((inner (cl-tron-mcp/json-compat:parse text)))
        (ok (equal "ready" (getf inner :|installation_status|)))
        (ok (eq t (getf inner :|compliance_verified|)))
        (ok (stringp (getf inner :|webpage|)))
        (ok (not (search ":SUCCESS" text)))
        (ok (not (getf (getf parsed :|result|) :|isError|))
            "success results are not marked isError")))))

(deftest tool-error-result-sets-iserror-test
  (testing "A tool that returns an error plist is reported with isError true"
    (multiple-value-bind (parsed text)
        (call-tool-via-protocol "skill_store_installation_guide" (list :|format| "json"))
      (let ((inner (cl-tron-mcp/json-compat:parse text)))
        (ok (eq t (getf (getf parsed :|result|) :|isError|)))
        (ok (stringp (getf inner :|message|)))))))

(deftest json-encoding-of-results-test
  (let ((encode (lambda (x)
                  (cl-tron-mcp/json-compat:to-json (cl-tron-mcp/tools::tool-result->json x)))))
    (testing "keys are lowercase snake_case, booleans and lists are encoded sensibly"
      (let ((parsed (cl-tron-mcp/json-compat:parse
                     (funcall encode (list :in-debugger nil :thread-count 3 :success t
                                           :names (list "a" "b") :items nil
                                           :nested (list :some-key :a-keyword))))))
        (ok (eq :false (getf parsed :|in_debugger|)))
        (ok (= 3 (getf parsed :|thread_count|)))
        (ok (eq t (getf parsed :|success|)))
        (ok (equal '("a" "b") (getf parsed :|names|)))
        (ok (null (getf parsed :|items|)) "an empty list stays an empty array")
        (ok (equal "a-keyword" (getf (getf parsed :|nested|) :|some_key|)))))
    (testing "a list of plists becomes an array of objects"
      (let ((parsed (cl-tron-mcp/json-compat:parse
                     (funcall encode (list :threads (list (list :id 1 :name "main")
                                                          (list :id 2 :name "worker")))))))
        (ok (= 2 (length (getf parsed :|threads|))))
        (ok (equal "worker" (getf (second (getf parsed :|threads|)) :|name|)))))
    (testing "values with no JSON form are printed rather than crashing"
      (ok (stringp (funcall encode (list :thing (make-symbol "WEIRD") :pair (cons 1 2))))))))

(defmacro with-temporary-tool ((name handler) &body body)
  `(progn
     (cl-tron-mcp/tools::register-tool ,name "temporary test tool" :requires-approval nil)
     (cl-tron-mcp/tools::register-tool-handler ,name ,handler)
     (unwind-protect (progn ,@body)
       (remhash ,name cl-tron-mcp/tools::*tool-registry*))))

(deftest tool-timeout-interrupts-hung-tool-test
  (testing "A tool that blocks longer than the timeout is abandoned and an error is returned"
    (with-temporary-tool ("zz_test_sleep" (lambda (&key &allow-other-keys) (sleep 30) (list :done t)))
      (let ((cl-tron-mcp/tools:*default-tool-timeout* 1)
            (start (get-internal-real-time)))
        (let* ((response (cl-tron-mcp/protocol::handle-tool-call
                          7 (list :|name| "zz_test_sleep" :|arguments| nil)))
               (parsed (cl-tron-mcp/json-compat:parse response))
               (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
          (ok (< elapsed 5) "returned long before the tool would have finished")
          (ok (getf parsed :|error|))
          (ok (= -32008 (getf (getf parsed :|error|) :|code|))))))))

(deftest tool-timeout-lets-fast-tool-finish-test
  (testing "A fast tool still returns its result"
    (with-temporary-tool ("zz_test_fast" (lambda (&key &allow-other-keys) (list :answer 42)))
      (multiple-value-bind (parsed text) (call-tool-via-protocol "zz_test_fast" nil)
        (declare (ignore parsed))
        (ok (= 42 (getf (cl-tron-mcp/json-compat:parse text) :|answer|)))))))

(deftest tool-errors-do-not-escape-worker-test
  (testing "An error inside the tool becomes a JSON-RPC error, not a crash"
    (with-temporary-tool ("zz_test_boom" (lambda (&key &allow-other-keys) (error "boom")))
      (let* ((response (cl-tron-mcp/protocol::handle-tool-call
                        8 (list :|name| "zz_test_boom" :|arguments| nil)))
             (parsed (cl-tron-mcp/json-compat:parse response)))
        (ok (getf parsed :|error|))
        (ok (search "boom" (getf (getf parsed :|error|) :|message|)))))))
