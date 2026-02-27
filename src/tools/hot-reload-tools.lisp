;;;; src/tools/hot-reload-tools.lisp
;;;; Hot-reload tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "code_compile_string"
  "Compile and load code string"
  :input-schema (list :code "string" :filename "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/code-compile-string.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when filename (validate-string "filename" filename)))
  :body (cl-tron-mcp/hot-reload:compile-and-load :code code :filename filename))

(define-validated-tool "reload_system"
  "Reload ASDF system"
  :input-schema (list :systemName "string" :force "boolean")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/reload-system.md"
  :validation ((validate-string "system_name" system_name :required t :min-length 1)
               (when force (validate-boolean "force" force)))
  :body (cl-tron-mcp/hot-reload:reload-system :system-name system_name :force force))