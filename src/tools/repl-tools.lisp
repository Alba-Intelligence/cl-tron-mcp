;;;; src/tools/repl-tools.lisp
;;;; REPL tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "repl_eval"
  "Evaluate Lisp code in REPL context. Use for testing, debugging, and modifying running code. Requires connection to Swank. Code runs in the persistent Lisp session."
  :input-schema (list :code "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-string "code" code :required t :min-length 1)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/repl:repl-eval :code code :package package))