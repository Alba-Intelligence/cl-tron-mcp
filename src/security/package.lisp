;;;; src/security/package.lisp

(defpackage :cl-tron-mcp/security
  (:use :cl)
  (:export
   #:*approval-required-operations*
   #:request-approval
   #:check-approval
   #:operation-requires-approval
   #:approval-response
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
   #:whitelist-check))
