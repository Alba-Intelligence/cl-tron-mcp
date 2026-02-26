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
   #:consume-approved-request-id
   #:generate-approval-request-id
   #:tool-name-to-operation
   #:whitelist-check-tool
   #:*audit-log*
   #:log-operation
   #:get-audit-log
   #:*pending-approvals*
   #:get-unix-time
   ;; Whitelist
   #:whitelist-add
   #:whitelist-remove
   #:whitelist-clear
   #:whitelist-enable
   #:whitelist-status
   #:whitelist-check
   ;; Cleanup
   #:cleanup-expired-approvals
   #:cleanup-old-approved-requests
   #:cleanup-all-approvals))
