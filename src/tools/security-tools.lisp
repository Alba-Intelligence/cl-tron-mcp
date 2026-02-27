;;;; src/tools/security-tools.lisp
;;;; Security/approval whitelist tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "whitelist_add"
  "Add whitelist pattern"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/whitelist-add.md"
  :validation ((validate-string "operation" operation :required t)
               (validate-string "pattern" pattern :required t))
  :body (cl-tron-mcp/security:whitelist-add :operation operation :pattern pattern))

(define-validated-tool "whitelist_remove"
  "Remove whitelist pattern"
  :input-schema (list :operation "string" :pattern "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/whitelist-remove.md"
  :validation ((validate-string "operation" operation :required t)
               (validate-string "pattern" pattern :required t))
  :body (cl-tron-mcp/security:whitelist-remove :operation operation :pattern pattern))

(define-validated-tool "whitelist_clear"
  "Clear whitelist"
  :input-schema (list :operation "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/whitelist-clear.md"
  :validation ((validate-string "operation" operation :required t))
  :body (cl-tron-mcp/security:whitelist-clear :operation operation))

(define-validated-tool "whitelist_enable"
  "Enable/disable whitelist"
  :input-schema (list :enable "boolean")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/whitelist-enable.md"
  :validation ((validate-boolean "enable" enable :required t))
  :body (cl-tron-mcp/security:whitelist-enable :enable enable))

(define-simple-tool "whitelist_status"
  "Get whitelist status"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/whitelist-status.md"
  :function cl-tron-mcp/security:whitelist-status)