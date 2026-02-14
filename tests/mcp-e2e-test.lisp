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

(deftest mcp-tools-list-test
  (testing "List tools"
    (let* ((response (cl-tron-mcp/protocol:handle-tools-list 1))
           (parsed (parse-json-response response)))
      (ok (getf parsed :|result|))
      (let ((result (getf parsed :|result|)))
        (ok (getf result :|tools|))
        (let ((tools (getf result :|tools|)))
          (ok (listp tools))
          (ok (> (length tools) 90) "Should have 99 tools"))))))

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
