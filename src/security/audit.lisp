;;;; src/security/audit.lisp

(in-package :cl-tron-mcp/security)

(defvar *audit-log* (make-array 1000 :fill-pointer 0 :adjustable t))
(defvar *audit-lock* (bt:make-lock "audit"))

(defstruct audit-entry
  timestamp
  operation
  tool
  user
  resource
  approved-p
  duration
  result)

(defun log-operation (operation tool &key (user "unknown") resource approved-p duration result)
  "Log an operation for audit trail."
  (bt:with-lock-held (*audit-lock*)
    (let ((entry (make-audit-entry
                  :timestamp (get-unix-time)
                  :operation operation
                  :tool tool
                  :user user
                  :resource resource
                  :approved-p approved-p
                  :duration duration
                  :result result)))
      (vector-push-extend entry *audit-log*))))

(defun get-audit-log (&key (limit 100))
  "Get recent audit log entries."
  (bt:with-lock-held (*audit-lock*)
    (let ((entries (subseq *audit-log* (max 0 (- (length *audit-log*) limit)))))
      (map 'list #'identity entries))))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))
