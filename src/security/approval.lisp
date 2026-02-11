;;;; src/security/approval.lisp

(in-package :cl-tron-mcp/security)

(defparameter *approval-required-operations*
  '(:eval :compile-file :modify-running-code :terminate-thread
    :set-breakpoint :trace-function :modify-restarts))

(defvar *pending-approvals* (make-hash-table :test 'equal))
(defvar *approval-lock* (bt:make-lock "approval"))
(defvar *approval-timeout* 60)

(defvar *approval-whitelist* (make-hash-table :test 'equal))
(defvar *whitelist-enabled* nil)

(defstruct approval-request id operation details timestamp expires response)

(defun operation-requires-approval (operation)
  (member operation *approval-required-operations*))

(defun whitelist-add (operation pattern)
  (bt:with-lock-held (*approval-lock*)
    (push pattern (gethash operation *approval-whitelist*))
    (list :operation operation :pattern pattern)))

(defun whitelist-remove (operation pattern)
  (bt:with-lock-held (*approval-lock*)
    (let ((patterns (gethash operation *approval-whitelist*)))
      (when patterns
        (setf (gethash operation *approval-whitelist*)
              (remove pattern patterns :test #'equal))))
    (list :operation operation :pattern pattern :removed t)))

(defun whitelist-clear (&optional operation)
  (bt:with-lock-held (*approval-lock*)
    (if operation
        (setf (gethash operation *approval-whitelist*) nil)
        (clrhash *approval-whitelist*))
    (list :cleared operation)))

(defun whitelist-enable (&optional (enable t))
  (setq *whitelist-enabled* enable)
  (list :whitelist-enabled enable))

(defun whitelist-status ()
  (list :enabled *whitelist-enabled*))

(defun whitelist-check (operation details)
  (declare (ignore operation details))
  *whitelist-enabled*)

(defun request-approval (operation details &key (timeout *approval-timeout*))
  (let ((request (make-approval-request
                  :operation operation
                  :details details
                  :timestamp (get-unix-time)
                  :expires (+ (get-unix-time) timeout))))
    (bt:with-lock-held (*approval-lock*)
      (setf (gethash (approval-request-id request) *pending-approvals*) request))
    request))

(defun approval-response (request-id &optional response)
  (bt:with-lock-held (*approval-lock*)
    (let ((request (gethash request-id *pending-approvals*)))
      (unless request
        (error "Approval request ~a not found" request-id))
      (when response
        (setf (approval-request-response request) response)
        (remhash request-id *pending-approvals*))
      (approval-request-response request))))

(defun check-approval (operation details)
  (when (whitelist-check operation details)
    (return-from check-approval t))
  (let ((request (request-approval operation details :timeout 0)))
    (loop for i from 0 to 10
          do (sleep 0.1)
             (let ((resp (approval-response (approval-request-id request))))
               (when resp
                 (return (eq resp :approved)))))))

(defun get-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))
