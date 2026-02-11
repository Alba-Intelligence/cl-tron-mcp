;;;; src/security/approval.lisp

(in-package :cl-tron-mcp/security)

(defparameter *approval-required-operations*
  '(:eval
    :compile-file
    :modify-running-code
    :terminate-thread
    :set-breakpoint
    :trace-function
    :modify-restarts))

(defvar *pending-approvals* (make-hash-table :test 'equal))
(defvar *approval-lock* (bt:make-lock "approval"))
(defvar *approval-timeout* 60)

(defstruct approval-request
  id
  operation
  details
  timestamp
  expires
  response)

(defun operation-requires-approval (operation)
  "Check if operation requires user approval."
  (member operation *approval-required-operations*))

(defun request-approval (operation details &key (timeout *approval-timeout*))
  "Request user approval for operation. Returns approval request."
  (let ((request (make-approval-request
                  :operation operation
                  :details (alexandria:plist-hash-table details)
                  :timestamp (get-unix-time)
                  :expires (+ (get-unix-time) timeout))))
    (bt:with-lock-held (*approval-lock*)
      (setf (gethash (approval-request-id request) *pending-approvals*) request))
    request))

(defun approval-response (request-id &optional response)
  "Get or set approval response."
  (bt:with-lock-held (*approval-lock*)
    (let ((request (gethash request-id *pending-approvals*)))
      (unless request
        (error "Approval request ~a not found" request-id))
      (when response
        (setf (approval-request-response request) response)
        (remhash request-id *pending-approvals*))
      (approval-request-response request))))

(defun check-approval (operation details)
  "Check if operation has approval. Returns t if approved, nil if pending."
  (let ((request (request-approval operation details :timeout 0)))
    (loop for i from 0 to 10
          do (sleep 0.1)
             (let ((resp (approval-response (approval-request-id request))))
               (when resp
                 (return (eq resp :approved)))))))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))
