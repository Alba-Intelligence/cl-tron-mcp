;;;; src/security/approval.lisp

(in-package :cl-tron-mcp/security)

(defparameter *approval-required-operations*
  '(:eval :compile-file :modify-running-code :terminate-thread
    :set-breakpoint :trace-function :modify-restarts :launch-process))

(defvar *pending-approvals* (make-hash-table :test 'equal))
(defvar *approval-lock* (bt:make-lock "approval"))
(defvar *approval-timeout* 300)

(defvar *approval-whitelist* (make-hash-table :test 'equal))
(defvar *whitelist-enabled* nil)
;; One-time use: when client sends approval/respond(approved: true), we add request_id here.
;; When tools/call is re-invoked with approval_request_id, we consume and run the tool.
(defvar *approved-request-ids* (make-hash-table :test 'equal))

;;; Atomic counter for unique request IDs
(defvar *approval-request-counter* 0
  "Atomic counter for generating unique approval request IDs.")

(defvar *approval-counter-lock* (bt:make-lock "approval-counter")
  "Lock for synchronizing access to *approval-request-counter*.")

;; BINDING is an opaque EQUAL-comparable value (e.g. a tool+arguments fingerprint) fixed at
;; request time. An approval can only be consumed by a call presenting the same binding.
;; CODE is the out-of-band confirmation secret (S4): it is shown only on the server's terminal and
;; never returned to the MCP client, so the agent cannot approve its own request.
(defstruct approval-request id operation details timestamp expires response actor approved-at binding
  code (failed-attempts 0))

(defvar *require-confirmation-code*
  (not (member (uiop:getenv "TRON_APPROVAL_TRUST_CLIENT") '("1" "true" "yes") :test #'equal))
  "When true (default), approving a request needs the confirmation code that Tron prints on its
own terminal. Setting TRON_APPROVAL_TRUST_CLIENT=1 restores the old behaviour in which whoever can
call approval/respond (including the agent) can approve; use it only when the MCP client itself
withholds approval/respond from the model.")

(defvar *max-code-attempts* 3
  "Wrong confirmation codes allowed before the request is discarded.")

(defparameter +code-alphabet+ "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "Unambiguous characters (no 0/O, 1/I).")

(defun generate-confirmation-code (&optional (length 8))
  "Return a random code from the OS entropy source (falls back to a time-seeded PRNG)."
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (handler-case
        (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
          (read-sequence bytes in))
      (error ()
        (let ((state (make-random-state t)))
          (dotimes (i length) (setf (aref bytes i) (random 256 state))))))
    (map 'string (lambda (b) (char +code-alphabet+ (mod b (length +code-alphabet+)))) bytes)))

(defun constant-time-string= (a b)
  "Compare strings without leaking where they differ."
  (and (= (length a) (length b))
       (zerop (loop for x across a for y across b
                    sum (logxor (char-code x) (char-code y))))))

(defun announce-approval-request (request)
  "Show REQUEST and its confirmation code to the human: on stderr (never stdout, which carries
JSON-RPC in stdio mode) and on the controlling terminal when there is one."
  (let* ((details (let ((*print-pretty* nil)) (princ-to-string (approval-request-details request))))
         (text (format nil "~&[tron] APPROVAL REQUIRED  request ~a  operation ~(~a~)~%[tron]   ~a~%[tron]   confirmation code: ~a   (valid for ~a seconds)~%"
                       (approval-request-id request)
                       (approval-request-operation request)
                       (subseq details 0 (min 300 (length details)))
                       (approval-request-code request)
                       (max 0 (- (approval-request-expires request) (get-unix-time))))))
    (ignore-errors (write-string text *error-output*) (finish-output *error-output*))
    (ignore-errors
     (with-open-file (tty "/dev/tty" :direction :output :if-exists :append)
       (write-string text tty)))))

(defun operation-requires-approval (operation)
  (member operation *approval-required-operations*))

(defun coerce-operation-key (operation)
  "Normalize OPERATION to a keyword regardless of whether it's a string or keyword."
  (etypecase operation
    (keyword operation)
    (string (intern (string-upcase operation) :keyword))))

(defun whitelist-add (operation pattern)
  (let ((op (coerce-operation-key operation)))
    (bt:with-lock-held (*approval-lock*)
      (push pattern (gethash op *approval-whitelist*))
      (list :operation op :pattern pattern))))

(defun whitelist-remove (operation pattern)
  (let ((op (coerce-operation-key operation)))
    (bt:with-lock-held (*approval-lock*)
      (let ((patterns (gethash op *approval-whitelist*)))
        (when patterns
          (setf (gethash op *approval-whitelist*)
                (remove pattern patterns :test #'equal))))
      (list :operation op :pattern pattern :removed t))))

(defun whitelist-clear (&optional operation)
  (bt:with-lock-held (*approval-lock*)
    (if operation
        (let ((op (coerce-operation-key operation)))
          (setf (gethash op *approval-whitelist*) nil))
        (clrhash *approval-whitelist*))
    (list :cleared operation)))

(defun whitelist-enable (&optional (enable t))
  (setq *whitelist-enabled* enable)
  (list :whitelist-enabled enable))

(defun whitelist-status ()
  (list :enabled *whitelist-enabled*))

(defun glob-match-p (pattern string)
  "True if STRING matches PATTERN, case-insensitively and anchored at both ends.
* in PATTERN matches any run of characters (including none); everything else is literal."
  (labels ((match (pj si)
             (cond ((= pj (length pattern)) (= si (length string)))
                   ((char= (char pattern pj) #\*)
                    (loop for k from si to (length string)
                          thereis (match (1+ pj) k)))
                   ((and (< si (length string))
                         (char-equal (char pattern pj) (char string si)))
                    (match (1+ pj) (1+ si)))
                   (t nil))))
    (match 0 0)))

(defun whitelist-match-p (pattern details)
  "Return t if DETAILS matches PATTERN.
PATTERN can be: t (wildcard), \"*\" (wildcard), a function (called with details), or a string.
A string is an EXACT, case-insensitive match against the tool name (the :tool of a plist
DETAILS, or DETAILS itself when it is a string); use * for globs, e.g. \"repl_*\". It is never
a substring match."
  (cond
    ((eq pattern t) t)
    ((functionp pattern) (funcall pattern details))
    ((stringp pattern)
     (let ((target (cond
                     ((stringp details) details)
                     ((listp details) (or (getf details :tool)
                                          (getf details :operation-details)
                                          ""))
                     (t ""))))
       (glob-match-p pattern (or target ""))))
    (t nil)))

(defun whitelist-check (operation details)
  "Return t if OPERATION with DETAILS is whitelisted (no user approval needed).
Checks stored patterns for the given operation."
  (when *whitelist-enabled*
    (bt:with-lock-held (*approval-lock*)
      (let ((patterns (gethash operation *approval-whitelist*)))
        (some (lambda (pattern) (whitelist-match-p pattern details))
              (or patterns '()))))))

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
                    ("repl_abort" . :terminate-thread)
                    ("swank_launch" . :launch-process) ("swank_kill" . :launch-process)))
      (setf (gethash (car pair) h) (cdr pair)))
    h))

(defun tool-name-to-operation (tool-name)
  (gethash (string tool-name) *tool-name-to-operation*))

(defun whitelist-check-tool (tool-name arguments)
  "Return t if this tool/arguments are whitelisted (no user approval needed)."
  (let ((op (tool-name-to-operation tool-name)))
    (when (and op (whitelist-check op (list :tool tool-name :arguments arguments)))
      (log-operation :whitelist-bypass (string tool-name)
                     :resource arguments :approved-p t :result :whitelisted)
      t)))

(defun generate-approval-request-id ()
  "Return a unique string ID for an approval request (JSON-RPC safe).
Uses atomic counter to ensure uniqueness even when called concurrently."
  (bt:with-lock-held (*approval-counter-lock*)
    (incf *approval-request-counter*)
    (format nil "~a-~a-~a"
            (get-unix-time)
            *approval-request-counter*
            (random 10000))))

(defun request-approval (operation details &key (timeout *approval-timeout*) (actor "unknown") binding)
  "Create an approval request. ACTOR identifies who/what is requesting approval.
BINDING pins the approval to one specific call (see CONSUME-APPROVED-REQUEST-ID)."
  (cleanup-all-approvals)
  (let* ((req-id (generate-approval-request-id))
         (now (get-unix-time))
         (request (make-approval-request
                   :id req-id
                   :operation operation
                   :details details
                   :timestamp now
                   :expires (+ now timeout)
                   :actor actor
                   :binding binding
                   :code (when *require-confirmation-code* (generate-confirmation-code)))))
    (bt:with-lock-held (*approval-lock*)
      (setf (gethash req-id *pending-approvals*) request))
    (when (approval-request-code request)
      (announce-approval-request request))
    request))

(defun approval-response (request-id &optional response &key (actor "user") code)
  "Record user response for request-id. ACTOR identifies who approved/denied.
Signals an error if the request is unknown or has expired (the timeout is enforced here).
Approving a request that carries a confirmation code (the default) requires CODE to match; a
wrong code signals an error, and after *MAX-CODE-ATTEMPTS* wrong codes the request is discarded.
Denying never needs a code. When response is :approved, the approval (with its expiry and binding) is stored in
*approved-request-ids* for one-time consumption."
  (bt:with-lock-held (*approval-lock*)
    (let ((request (gethash request-id *pending-approvals*)))
      (unless request
        (error "Approval request ~a not found" request-id))
      (when (> (get-unix-time) (approval-request-expires request))
        (remhash request-id *pending-approvals*)
        (error "Approval request ~a has expired; invoke the tool again" request-id))
      (when (and (eq response :approved) (approval-request-code request))
        (unless (and (stringp code)
                     (constant-time-string= (string-upcase (string-trim " " code))
                                            (approval-request-code request)))
          (when (>= (incf (approval-request-failed-attempts request)) *max-code-attempts*)
            (remhash request-id *pending-approvals*))
          (error "Invalid or missing confirmation code. The code is shown on the Tron server's terminal, not in the tool result.")))
      (when response
        (setf (approval-request-response request) response)
        (setf (approval-request-approved-at request) (get-unix-time))
        (setf (approval-request-actor request) actor)
        (remhash request-id *pending-approvals*)
        (when (eq response :approved)
          (setf (gethash request-id *approved-request-ids*)
                (list :time (get-unix-time)
                      :expires (approval-request-expires request)
                      :binding (approval-request-binding request)))))
      (approval-request-response request))))

(defun consume-approved-request-id (request-id &optional (binding nil binding-p))
  "One-time consume of an approved REQUEST-ID. Returns T only if the approval exists, has not
expired, and (when BINDING is supplied) its stored binding is EQUAL to BINDING. The ID is
removed on any lookup hit, so a mismatched or expired attempt also burns the approval."
  (bt:with-lock-held (*approval-lock*)
    (let ((entry (gethash request-id *approved-request-ids*)))
      (when entry
        (remhash request-id *approved-request-ids*)
        (and (<= (get-unix-time) (getf entry :expires))
             (or (not binding-p)
                 (equal binding (getf entry :binding))))))))

(defun cleanup-expired-approvals ()
  "Remove expired approval requests from *pending-approvals*."
  (bt:with-lock-held (*approval-lock*)
    (let ((now (get-unix-time))
          (expired-count 0))
      (maphash (lambda (req-id request)
                 (when (> now (approval-request-expires request))
                   (remhash req-id *pending-approvals*)
                   (incf expired-count)))
               *pending-approvals*)
      (when (> expired-count 0)
        (cl-tron-mcp/logging:log-info
         (format nil "Cleaned up ~d expired approval requests" expired-count)))
      expired-count)))

(defun cleanup-old-approved-requests (&optional (max-age 300))
  "Remove approved request IDs older than MAX-AGE seconds (default 5 minutes)."
  (bt:with-lock-held (*approval-lock*)
    (let ((now (get-unix-time))
          (expired-count 0))
      (maphash (lambda (req-id entry)
                 (when (or (> (- now (getf entry :time)) max-age)
                           (> now (getf entry :expires)))
                   (remhash req-id *approved-request-ids*)
                   (incf expired-count)))
               *approved-request-ids*)
      (when (> expired-count 0)
        (cl-tron-mcp/logging:log-info
         (format nil "Cleaned up ~d old approved request IDs" expired-count)))
      expired-count)))

(defun cleanup-all-approvals ()
  "Clean up expired pending approvals and old approved request IDs."
  (let ((expired (cleanup-expired-approvals))
        (old-approved (cleanup-old-approved-requests)))
    (list :expired-pending expired
          :expired-approved old-approved)))

(defun check-approval (operation details)
  "Return t if operation is approved (whitelisted). Server-enforced approval uses
   approval_required response and approval/respond instead of blocking here."
  (whitelist-check operation details))

(defun get-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))
