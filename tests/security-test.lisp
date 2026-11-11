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
    (testing "Whitelist string patterns are exact or * globs"
             (cl-tron-mcp/security:whitelist-clear)
             (cl-tron-mcp/security:whitelist-enable t)
             (cl-tron-mcp/security:whitelist-add :eval "safe-prefix*")
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

;;; Approval binding / expiry (regression tests for the approval-replay finding)

(deftest approval-bound-to-call-test
  (testing "An approval only validates the exact call it was requested for"
    (let* ((cl-tron-mcp/security:*require-confirmation-code* nil)
           (req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "call-A"))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (cl-tron-mcp/security:approval-response id :approved)
      (ok (not (cl-tron-mcp/security:consume-approved-request-id id "call-B")))
      (ok (not (cl-tron-mcp/security:consume-approved-request-id id "call-A"))
          "a mismatched attempt burns the approval"))
    (let* ((cl-tron-mcp/security:*require-confirmation-code* nil)
           (req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "call-A"))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (cl-tron-mcp/security:approval-response id :approved)
      (ok (cl-tron-mcp/security:consume-approved-request-id id "call-A"))
      (ok (not (cl-tron-mcp/security:consume-approved-request-id id "call-A"))
          "approvals are single-use"))))

(deftest approval-expiry-test
  (testing "Expired approval requests cannot be approved"
    (let* ((cl-tron-mcp/security:*require-confirmation-code* nil)
           (req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :timeout -1))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (signals (cl-tron-mcp/security:approval-response id :approved)))))

(deftest approval-binding-fingerprint-test
  (testing "Binding differs when tool or arguments differ, ignores approval params"
    (let ((a (cl-tron-mcp/tools::approval-binding "repl_eval" '(:|code| "(+ 1 2)")))
          (b (cl-tron-mcp/tools::approval-binding "repl_eval" '(:|code| "(+ 1 3)")))
          (c (cl-tron-mcp/tools::approval-binding "swank_eval" '(:|code| "(+ 1 2)")))
          (d (cl-tron-mcp/tools::approval-binding
              "repl_eval" '(:|code| "(+ 1 2)" :|approval_request_id| "x" :|approved| t))))
      (ok (string/= a b))
      (ok (string/= a c))
      (ok (string= a d)))))

;;; Out-of-band confirmation (S4): the agent must not be able to approve its own requests.

(defun pending-code (id)
  "What the human sees on the server terminal; never part of the tool result."
  (cl-tron-mcp/security::approval-request-code
   (gethash id cl-tron-mcp/security::*pending-approvals*)))

(deftest approval-needs-confirmation-code-test
  (testing "Approving without the out-of-band code is refused and the request stays pending"
    (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "c"))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (ok (stringp (pending-code id)))
      (signals (cl-tron-mcp/security:approval-response id :approved))
      (signals (cl-tron-mcp/security:approval-response id :approved :code "WRONG"))
      (ok (gethash id cl-tron-mcp/security::*pending-approvals*))
      (ok (not (cl-tron-mcp/security:consume-approved-request-id id "c")))))
  (testing "The right code approves"
    (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "c"))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (cl-tron-mcp/security:approval-response id :approved :code (pending-code id))
      (ok (cl-tron-mcp/security:consume-approved-request-id id "c"))))
  (testing "Code comparison ignores case and surrounding whitespace"
    (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "c"))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (cl-tron-mcp/security:approval-response
       id :approved :code (format nil " ~a " (string-downcase (pending-code id))))
      (ok (cl-tron-mcp/security:consume-approved-request-id id "c")))))

(deftest approval-code-guessing-is-limited-test
  (testing "Three wrong codes burn the request, even if the right code follows"
    (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "c"))
           (id (cl-tron-mcp/security:approval-request-id req))
           (code (pending-code id)))
      (dotimes (i 3)
        (signals (cl-tron-mcp/security:approval-response id :approved :code "WRONG")))
      (signals (cl-tron-mcp/security:approval-response id :approved :code code))
      (ok (not (gethash id cl-tron-mcp/security::*pending-approvals*))))))

(deftest approval-denial-needs-no-code-test
  (testing "Denying is always allowed without a code"
    (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval")))
           (id (cl-tron-mcp/security:approval-request-id req)))
      (ok (eq :denied (cl-tron-mcp/security:approval-response id :denied))))))

(deftest approval-trust-client-mode-test
  (testing "With *require-confirmation-code* nil (explicit opt-out) no code is needed"
    (let ((cl-tron-mcp/security:*require-confirmation-code* nil))
      (let* ((req (cl-tron-mcp/security:request-approval :eval '(:tool "repl_eval") :binding "c"))
             (id (cl-tron-mcp/security:approval-request-id req)))
        (cl-tron-mcp/security:approval-response id :approved)
        (ok (cl-tron-mcp/security:consume-approved-request-id id "c"))))))

;;; Whitelist matching (S5): exact tool names or explicit globs, never substrings; hits are audited.

(deftest whitelist-exact-and-glob-test
  (testing "A bare pattern is an exact tool-name match, not a substring"
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable t)
    (cl-tron-mcp/security:whitelist-add :eval "repl")
    (ok (not (cl-tron-mcp/security:whitelist-check-tool "repl_eval" nil)))
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-add :eval "repl_eval")
    (ok (cl-tron-mcp/security:whitelist-check-tool "repl_eval" nil))
    (ok (not (cl-tron-mcp/security:whitelist-check-tool "swank_eval" nil)))
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable nil))
  (testing "* is a glob, anchored at both ends"
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable t)
    (cl-tron-mcp/security:whitelist-add :eval "repl_*")
    (ok (cl-tron-mcp/security:whitelist-check-tool "repl_eval" nil))
    (ok (not (cl-tron-mcp/security:whitelist-check-tool "swank_eval" nil)))
    (ok (not (cl-tron-mcp/security:whitelist-check-tool "xrepl_eval" nil)))
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable nil))
  (testing "Matching is case-insensitive"
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable t)
    (cl-tron-mcp/security:whitelist-add :eval "REPL_EVAL")
    (ok (cl-tron-mcp/security:whitelist-check-tool "repl_eval" nil))
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable nil)))

(deftest whitelist-bypass-is-audited-test
  (testing "A whitelist hit writes an audit entry; a miss does not"
    (cl-tron-mcp/security:clear-audit-log)
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable t)
    (cl-tron-mcp/security:whitelist-add :eval "repl_eval")
    (cl-tron-mcp/security:whitelist-check-tool "swank_eval" nil)
    (ok (null (cl-tron-mcp/security:get-audit-log)))
    (cl-tron-mcp/security:whitelist-check-tool "repl_eval" (list :|code| "1"))
    (let ((log (cl-tron-mcp/security:get-audit-log)))
      (ok (= 1 (length log)))
      (ok (equal "repl_eval" (cl-tron-mcp/security::audit-entry-tool (first log))))
      (ok (eq :whitelist-bypass (cl-tron-mcp/security::audit-entry-operation (first log)))))
    (cl-tron-mcp/security:whitelist-clear)
    (cl-tron-mcp/security:whitelist-enable nil)))
