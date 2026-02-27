;;;; src/tools/security-tools.lisp
;;;; Security/approval whitelist tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "whitelist_add"
  "Add a pattern to the approval whitelist"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "operation" operation :required t)
               (validate-string "pattern" pattern :required t))
  :body (cl-tron-mcp/security:whitelist-add :operation operation :pattern pattern))

(define-validated-tool "whitelist_remove"
  "Remove a pattern from the approval whitelist"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "operation" operation :required t)
               (validate-string "pattern" pattern :required t))
  :body (cl-tron-mcp/security:whitelist-remove :operation operation :pattern pattern))

(define-validated-tool "whitelist_clear"
  "Clear the approval whitelist"
  :input-schema (list :operation "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "operation" operation :required t))
  :body (cl-tron-mcp/security:whitelist-clear :operation operation))

(define-validated-tool "whitelist_enable"
  "Enable or disable the approval whitelist"
  :input-schema (list :enable "boolean")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-boolean "enable" enable :required t))
  :body (cl-tron-mcp/security:whitelist-enable :enable enable))

(define-simple-tool "whitelist_status"
  "Get current whitelist status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/security:whitelist-status)