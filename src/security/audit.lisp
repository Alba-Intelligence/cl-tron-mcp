;;;; src/security/audit.lisp

(in-package :cl-tron-mcp/security)

(defvar *audit-log-max-size* 5000
  "Maximum number of audit entries to retain in memory.")

(defvar *audit-log* (make-array 256 :fill-pointer 0 :adjustable t))
(defvar *audit-lock* (bt:make-lock "audit"))

;;; Parameter names whose values should be redacted in logs.
(defvar *sensitive-parameter-names*
  '("password" "secret" "token" "key" "credential" "auth" "api_key" "apikey"
    "access_token" "private_key" "passphrase")
  "List of parameter name substrings (case-insensitive) that should be redacted in logs.")

(defstruct audit-entry
  timestamp
  operation
  tool
  user
  resource
  approved-p
  duration
  result)

(defun sensitive-param-p (name)
  "Return t if NAME matches any sensitive parameter name pattern."
  (let ((name-lower (string-downcase (string name))))
    (some (lambda (sensitive)
            (search sensitive name-lower :test #'char-equal))
          *sensitive-parameter-names*)))

(defun sanitize-arguments (args)
  "Sanitize a plist of tool arguments, redacting sensitive values.
Returns a new plist with sensitive parameter values replaced by \"[REDACTED]\"."
  (cond
    ((null args) nil)
    ((listp args)
     (loop for (key val) on args by #'cddr
           when key
             append (list key (if (and (or (symbolp key) (stringp key))
                                       (sensitive-param-p key))
                                  "[REDACTED]"
                                  val))))
    (t args)))

(defun log-operation (operation tool &key (user "unknown") resource approved-p duration result)
  "Log an operation for audit trail. Sensitive parameter values are redacted.
Automatically trims log when it exceeds *audit-log-max-size* by dropping oldest entries."
  (bt:with-lock-held (*audit-lock*)
    (let ((entry (make-audit-entry
                  :timestamp (get-unix-time)
                  :operation operation
                  :tool tool
                  :user user
                  :resource (sanitize-arguments resource)
                  :approved-p approved-p
                  :duration duration
                  :result result)))
      (vector-push-extend entry *audit-log*)
      ;; Trim when over limit: keep the newest 3/4 of max-size
      (when (>= (length *audit-log*) *audit-log-max-size*)
        (let* ((keep-from (floor *audit-log-max-size* 4))
               (kept (subseq *audit-log* keep-from))
               (new-log (make-array (length kept)
                                    :fill-pointer (length kept)
                                    :adjustable t
                                    :initial-contents kept)))
          (setf *audit-log* new-log))))))

(defun get-audit-log (&key (limit 100))
  "Get the most recent LIMIT audit log entries."
  (bt:with-lock-held (*audit-lock*)
    (let ((len (length *audit-log*)))
      (map 'list #'identity
           (subseq *audit-log* (max 0 (- len limit)))))))

(defun clear-audit-log ()
  "Clear all audit log entries."
  (bt:with-lock-held (*audit-lock*)
    (setf *audit-log* (make-array 256 :fill-pointer 0 :adjustable t))))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))
