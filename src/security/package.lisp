;;;; src/security/package.lisp

(defpackage :cl-tron-mcp/security
  (:use :cl)
  (:export
   #:*approval-required-operations*
   #:request-approval
   #:check-approval
   #:operation-requires-approval
   #:approval-response
   #:approval-request-id
   #:approval-request-actor
   #:approval-request-approved-at
   #:consume-approved-request-id
   #:generate-approval-request-id
   #:tool-name-to-operation
   #:whitelist-check-tool
   #:*audit-log*
   #:*audit-log-max-size*
   #:*sensitive-parameter-names*
   #:log-operation
   #:get-audit-log
   #:clear-audit-log
   #:sanitize-arguments
   #:sensitive-param-p
   #:*pending-approvals*
   #:get-unix-time
   ;; Whitelist
   #:whitelist-add
   #:whitelist-remove
   #:whitelist-clear
   #:whitelist-enable
   #:whitelist-status
   #:whitelist-check
   #:whitelist-match-p
   #:cleanup-expired-approvals
   #:cleanup-old-approved-requests
   #:cleanup-all-approvals))
