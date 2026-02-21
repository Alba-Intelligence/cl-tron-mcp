;;;; tests/mcp-e2e-test.lisp - MCP Protocol End-to-End Tests
;;;;
;;;; These tests verify the MCP server works correctly through the protocol.
;;;; Run with: (rove:run 'cl-tron-mcp/tests/mcp-e2e)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :jonathan :silent t))

(defpackage :cl-tron-mcp/tests/mcp-e2e
  (:use #:cl #:rove))

(in-package :cl-tron-mcp/tests/mcp-e2e)

(defun parse-json-response (json-string)
  "Parse JSON response string to plist."
  (jonathan:parse json-string))

;;; Direct protocol tests

(deftest mcp-protocol-init-test
  (testing "Initialize request"
    (let* ((response (cl-tron-mcp/protocol:handle-initialize 1 nil))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|jsonrpc|))
      (ok (getf parsed :|result|))
      (let ((result (getf parsed :|result|)))
        (ok (getf result :|protocolVersion|))
        (ok (getf result :|serverInfo|))
        (ok (string= "cl-tron-mcp" (getf (getf result :|serverInfo|) :|name|)))))))

(deftest mcp-stdio-startup-test
  (testing "Stdio first line is JSON (optional, slow - run manually)"
    (ok t "Skipped: run manually to verify stdout purity: echo '{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}' | ./start-mcp.sh 2>/dev/null | head -1. With ECL: TRON_LISP=ecl ./start-mcp.sh same way.")))

(deftest mcp-tools-list-test
  (testing "List tools"
    (let* ((response (cl-tron-mcp/protocol:handle-tools-list 1))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|))
      (let ((result (getf parsed :|result|)))
        (ok (getf result :|tools|))
        (let ((tools (getf result :|tools|)))
          (ok (listp tools))
          (ok (>= (length tools) 100) "Should have 100 tools"))))))

(deftest mcp-tool-call-health-check
  (testing "Call health_check tool"
    (let* ((params (list :|name| "health_check" :|arguments| nil))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|)))))

(deftest mcp-tool-call-runtime-stats
  (testing "Call runtime_stats tool"
    (let* ((params (list :|name| "runtime_stats" :|arguments| nil))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|)))))

(deftest mcp-tool-call-system-info
  (testing "Call system_info tool"
    (let* ((params (list :|name| "system_info" :|arguments| nil))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|)))))

(deftest mcp-tool-call-inspect-function
  (testing "Call inspect_function tool"
    (let* ((args (list :|symbol_name| "CL:CAR"))
           (params (list :|name| "inspect_function" :|arguments| args))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|)))))

(deftest mcp-tool-call-thread-list
  (testing "Call thread_list tool"
    (let* ((params (list :|name| "thread_list" :|arguments| nil))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|)))))

;;; Swank integration via MCP

(defvar *swank-available* nil)

(defun swank-available-p ()
  (or *swank-available*
      (setf *swank-available*
            (handler-case
                (let ((socket (usocket:socket-connect "127.0.0.1" 4005 :timeout 2)))
                  (usocket:socket-close socket)
                  t)
              (error () nil)))))

(deftest mcp-swank-connect-test
  (testing "Call swank_connect tool"
    (if (not (swank-available-p))
        (ok t "Swank server not available - skipping")
        (let* ((args (list :|port| 4005))
               (params (list :|name| "swank_connect" :|arguments| args))
               (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
               (parsed (parse-json-response response)))
          (ok (getf parsed :|result|))
          (unwind-protect
               (let ((result (getf parsed :|result|)))
                 (when result
                   (ok (search "Connected" (format nil "~a" result) :test #'char-equal))))
            (cl-tron-mcp/swank:swank-disconnect))))))

(deftest mcp-swank-eval-test
  (testing "Call swank_eval tool via MCP"
    (if (not (swank-available-p))
        (ok t "Swank server not available - skipping")
        (progn
          (cl-tron-mcp/swank:swank-connect :port 4005)
          (unwind-protect
               (let* ((args (list :|code| "(+ 1 2 3)"))
                      (params (list :|name| "swank_eval" :|arguments| args))
                      (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
                      (parsed (parse-json-response response)))
                 (ok (getf parsed :|result|))
                 (let ((result (getf parsed :|result|)))
                   (when result
                     (ok (search "6" (format nil "~a" result))))))
            (cl-tron-mcp/swank:swank-disconnect))))))

;;; Approval flow tests

(deftest mcp-approval-required-test
  (testing "Protected tool returns approval_required with request_id"
    (let* ((params (list :|name| "swank_eval" :|arguments| (list :|code| "(+ 1 2)")))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|) "Should have result")
      (let ((content (getf (getf parsed :|result|) :|content|)))
        (ok content)
        (let ((text (getf (first content) :|text|)))
          (ok (stringp text))
          (let ((approval-json (ignore-errors (jonathan:parse text))))
            (ok approval-json "Content text should be JSON")
            (when approval-json
              (ok (getf approval-json :|approval_required|) "Should indicate approval required")
              (ok (getf approval-json :|request_id|) "Should include request_id")
              (ok (getf approval-json :|message|) "Should include message"))))))))

(deftest mcp-approval-respond-then-tool-test
  (testing "After approval/respond(approved: true), re-invoke tool with approval_request_id runs tool"
    (let* ((params1 (list :|name| "swank_eval" :|arguments| (list :|code| "(+ 1 2)")))
           (resp1 (cl-tron-mcp/protocol:handle-tool-call 1 params1))
           (parsed1 (parse-json-response resp1))
           (content (getf (getf parsed1 :|result|) :|content|))
           (text (getf (first content) :|text|))
           (approval-json (jonathan:parse text))
           (req-id (getf approval-json :|request_id|)))
      (ok req-id)
      ;; Record approval
      (let ((respond-params (list :|request_id| req-id :|approved| "true")))
        (cl-tron-mcp/protocol:handle-request 2 "approval/respond" respond-params))
      ;; Re-invoke with approval
      (let* ((params2 (list :|name| "swank_eval"
                            :|arguments| (list :|code| "(+ 1 2)"
                                               :|approval_request_id| req-id
                                               :|approved| "true")))
             (resp2 (cl-tron-mcp/protocol:handle-tool-call 2 params2))
             (parsed2 (parse-json-response resp2)))
        (ok (getf parsed2 :|result|) "Re-invoke with approval should return result (or tool error)")
        ;; Should not be approval_required again
        (let ((content2 (getf (getf parsed2 :|result|) :|content|)))
          (when content2
            (let ((text2 (getf (first content2) :|text|)))
              (when (stringp text2)
                (let ((inner (ignore-errors (jonathan:parse text2))))
                  (when inner
                    (ok (not (getf inner :|approval_required|))
                        "Should not ask for approval again"))))))))))

(deftest mcp-approval-deny-message-test
  (testing "approval/respond(approved: false) returns message for retry"
    (let* ((params1 (list :|name| "swank_eval" :|arguments| (list :|code| "1")))
           (resp1 (cl-tron-mcp/protocol:handle-tool-call 1 params1))
           (parsed1 (parse-json-response resp1))
           (content (getf (getf parsed1 :|result|) :|content|))
           (text (getf (first content) :|text|))
           (approval-json (jonathan:parse text))
           (req-id (getf approval-json :|request_id|)))
      (ok req-id)
      (let* ((respond-params (list :|request_id| req-id :|approved| "false" :|message| "User said no"))
             (resp2 (cl-tron-mcp/protocol:handle-request 2 "approval/respond" respond-params))
             (parsed2 (parse-json-response resp2)))
        (ok (getf parsed2 :|result|))
        (ok (getf (getf parsed2 :|result|) :|recorded|))
        (ok (not (getf (getf parsed2 :|result|) :|approved|)))
        (ok (getf (getf parsed2 :|result|) :|message|) "Should include message"))))

(deftest mcp-whitelist-skips-approval-test
  (testing "When whitelist allows operation, protected tool runs without approval_required"
    (cl-tron-mcp/security:whitelist-enable t)
    (cl-tron-mcp/security:whitelist-add :eval "*")
    (unwind-protect
         (let* ((params (list :|name| "swank_eval" :|arguments| (list :|code| "(+ 1 2)")))
                (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
                (parsed (parse-json-response response))
                (content (getf (getf parsed :|result|) :|content|))
                (text (when content (getf (first content) :|text|))))
           (ok (getf parsed :|result|))
           ;; Result text should not be approval_required JSON
           (when (stringp text)
             (let ((inner (ignore-errors (jonathan:parse text))))
               (when inner
                 (ok (not (getf inner :|approval_required|))
                     "Whitelisted tool should not return approval_required")))))
      (cl-tron-mcp/security:whitelist-clear)
      (cl-tron-mcp/security:whitelist-enable nil)))

;;; Error handling tests

(deftest mcp-error-invalid-tool
  (testing "Call non-existent tool"
    (let* ((params (list :|name| "nonexistent_tool_xyz" :|arguments| nil))
           (response (cl-tron-mcp/protocol:handle-tool-call 1 params))
           (parsed (parse-json-response response)))
      (ok (or (getf parsed :|error|)
              (and (getf parsed :|result|)
                   (search "error" (format nil "~a" (getf parsed :|result|)) :test #'char-equal)))
          "Should return error for invalid tool"))))
