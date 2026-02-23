;;;; src/security/approval.lisp

(in-package :cl-tron-mcp/security)

(defparameter *approval-required-operations*
  '(:eval :compile-file :modify-running-code :terminate-thread
    :set-breakpoint :trace-function :modify-restarts))

(defvar *pending-approvals* (make-hash-table :test 'equal))
(defvar *approval-lock* (bt:make-lock "approval"))
(defvar *approval-timeout* 300)

(defvar *approval-whitelist* (make-hash-table :test 'equal))
(defvar *whitelist-enabled* nil)
;; One-time use: when client sends approval/respond(approved: true), we add request_id here.
;; When tools/call is re-invoked with approval_request_id, we consume and run the tool.
(defvar *approved-request-ids* (make-hash-table :test 'equal))

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

;; Map MCP tool name (string) to approval operation keyword for whitelist lookup.
(defparameter *tool-name-to-operation*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair '(("repl_eval" . :eval) ("repl_compile" . :compile-file)
                    ("swank_eval" . :eval) ("swank_compile" . :compile-file)
                    ("code_compile_string" . :compile-file) ("reload_system" . :modify-running-code)
                    ("profile_start" . :modify-running-code) ("profile_stop" . :modify-running-code)
                    ("trace_function" . :trace-function) ("trace_remove" . :trace-function)
                    ("breakpoint_set" . :set-breakpoint) ("repl_set_breakpoint" . :set-breakpoint)
                    ("swank_abort" . :terminate-thread) ("swank_interrupt" . :terminate-thread)
                    ("repl_abort" . :terminate-thread)))
      (setf (gethash (car pair) h) (cdr pair)))
    h))

(defun tool-name-to-operation (tool-name)
  (gethash (string tool-name) *tool-name-to-operation*))

(defun whitelist-check-tool (tool-name arguments)
  "Return t if this tool/arguments are whitelisted (no user approval needed)."
  (let ((op (tool-name-to-operation tool-name)))
    (when op
      (whitelist-check op (list :tool tool-name :arguments arguments)))))

(defun generate-approval-request-id ()
  "Return a unique string ID for an approval request (JSON-RPC safe)."
  (format nil "~a-~a" (get-unix-time) (random 1000000)))

(defun request-approval (operation details &key (timeout *approval-timeout*))
  (let* ((req-id (generate-approval-request-id))
         (now (get-unix-time))
         (request (make-approval-request
                   :id req-id
                   :operation operation
                   :details details
                   :timestamp now
                   :expires (+ now timeout))))
    (bt:with-lock-held (*approval-lock*)
      (setf (gethash req-id *pending-approvals*) request))
    request))

(defun approval-response (request-id &optional response)
  "Record user response for request-id. When response is :approved, add to *approved-request-ids*
   for one-time use when the client re-invokes the tool with approval_request_id."
  (bt:with-lock-held (*approval-lock*)
    (let ((request (gethash request-id *pending-approvals*)))
      (unless request
        (error "Approval request ~a not found" request-id))
      (when response
        (setf (approval-request-response request) response)
        (remhash request-id *pending-approvals*)
        (when (eq response :approved)
          (setf (gethash request-id *approved-request-ids*) t)))
      (approval-request-response request))))

(defun consume-approved-request-id (request-id)
  "If request-id was approved (one-time), remove it and return t. Otherwise return nil."
  (bt:with-lock-held (*approval-lock*)
    (when (gethash request-id *approved-request-ids*)
      (remhash request-id *approved-request-ids*)
      t)))

(defun check-approval (operation details)
  "Return t if operation is approved (whitelisted). Server-enforced approval uses
   approval_required response and approval/respond instead of blocking here."
  (whitelist-check operation details))

(defun get-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))
