;;;; tests/process-manager-test.lisp — Unit tests for Swank process management

(in-package :cl-tron-mcp/tests)

;;; ============================================================
;;; Port Availability Tests
;;; ============================================================

(deftest port-available-p-test
    (testing "port-available-p returns nil for a closed port"
             (ok (not (cl-tron-mcp/swank:port-available-p 19999))))
  (testing "port-available-p returns t for an open port (Swank on 4006 if present)"
           (let ((swank-up (ignore-errors
                             (let ((s (usocket:socket-connect "127.0.0.1" 4006 :timeout 1)))
                               (usocket:socket-close s)
                               t))))
             (if swank-up
                 (ok (cl-tron-mcp/swank:port-available-p 4006))
                 (ok t "Swank not on 4006 - skip live check")))))

;;; ============================================================
;;; Process Registry Tests
;;; ============================================================

(deftest managed-processes-registry-test
    (testing "*managed-processes* is a hash table"
             (ok (hash-table-p cl-tron-mcp/swank:*managed-processes*)))
  (testing "list-managed-processes returns :success t with empty list when nothing launched"
           (let ((before-count (hash-table-count cl-tron-mcp/swank:*managed-processes*)))
             (when (= before-count 0)
               (let ((result (cl-tron-mcp/swank:list-managed-processes)))
                 (ok (getf result :success))
                 (ok (listp (getf result :processes))))))))

;;; ============================================================
;;; Kill Non-Existent Process
;;; ============================================================

(deftest kill-nonexistent-process-test
    (testing "kill-managed-process on unknown port returns error"
             (let ((result (cl-tron-mcp/swank:kill-managed-process :port 19998)))
               (ok (getf result :error))
               (ok (string= (getf result :code) "PROCESS_NOT_FOUND")))))

;;; ============================================================
;;; Status of Non-Existent Process
;;; ============================================================

(deftest process-status-nonexistent-test
    (testing "managed-process-status on unknown port returns error"
             (let ((result (cl-tron-mcp/swank:managed-process-status :port 19997)))
               (ok (getf result :error))
               (ok (string= (getf result :code) "PROCESS_NOT_FOUND")))))

;;; ============================================================
;;; Port Already In Use Guard
;;; ============================================================

(deftest launch-port-already-in-use-test
    (testing "launch-sbcl-with-swank returns error when port already open"
             (let ((swank-up (ignore-errors
                               (let ((s (usocket:socket-connect "127.0.0.1" 4006 :timeout 1)))
                                 (usocket:socket-close s)
                                 t))))
               (if swank-up
                   (let ((result (cl-tron-mcp/swank:launch-sbcl-with-swank :port 4006 :timeout 5)))
                     (ok (getf result :error))
                     (ok (string= (getf result :code) "PORT_ALREADY_IN_USE")))
                   (ok t "Swank not on 4006 - skip")))))

;;; ============================================================
;;; MCP Tool Wiring
;;; ============================================================

(deftest process-tools-registered-test
    (testing "swank_launch tool is registered"
             (ok (gethash "swank_launch" cl-tron-mcp/tools:*tool-registry*)))
  (testing "swank_kill tool is registered"
           (ok (gethash "swank_kill" cl-tron-mcp/tools:*tool-registry*)))
  (testing "swank_process_list tool is registered"
           (ok (gethash "swank_process_list" cl-tron-mcp/tools:*tool-registry*)))
  (testing "swank_process_status tool is registered"
           (ok (gethash "swank_process_status" cl-tron-mcp/tools:*tool-registry*)))
  (testing "swank_launch requires approval"
           (let ((entry (gethash "swank_launch" cl-tron-mcp/tools:*tool-registry*)))
             (when entry
               (ok (gethash :requiresApproval (cl-tron-mcp/tools:tool-entry-descriptor entry))))))
  (testing "swank_kill requires approval"
           (let ((entry (gethash "swank_kill" cl-tron-mcp/tools:*tool-registry*)))
             (when entry
               (ok (gethash :requiresApproval (cl-tron-mcp/tools:tool-entry-descriptor entry))))))
  (testing "swank_process_list does not require approval"
           (let ((entry (gethash "swank_process_list" cl-tron-mcp/tools:*tool-registry*)))
             (when entry
               (ok (not (gethash :requiresApproval
                                 (cl-tron-mcp/tools:tool-entry-descriptor entry))))))))

;;; ============================================================
;;; Security: :launch-process in approval operations
;;; ============================================================

(deftest launch-process-requires-approval-test
    (testing ":launch-process is in *approval-required-operations*"
             (ok (member :launch-process cl-tron-mcp/security:*approval-required-operations*)))
  (testing "swank_launch maps to :launch-process operation"
           (ok (eq :launch-process
                   (cl-tron-mcp/security:tool-name-to-operation "swank_launch"))))
  (testing "swank_kill maps to :launch-process operation"
           (ok (eq :launch-process
                   (cl-tron-mcp/security:tool-name-to-operation "swank_kill")))))
