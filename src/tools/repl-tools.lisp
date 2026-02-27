;;;; src/tools/repl-tools.lisp
;;;; REPL tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "repl_eval"
  "Evaluate code in REPL"
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/repl-eval-repl.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/repl:repl-eval :code code :package package))