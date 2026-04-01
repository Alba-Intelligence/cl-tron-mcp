;;;; src/tools/hot-reload-tools.lisp
;;;; Hot-reload tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "code_compile_string"
  "Compile and load a code string into the connected Lisp image via Swank. Falls back to local compilation when not connected."
  :input-schema (list :code "string" :filename "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/code-compile-string.md"
  :validation ((validate-string "code" code :required t :min-length 1)
               (when filename (validate-string "filename" filename))
               (when package (validate-string "package" package)))
  :body (if (cl-tron-mcp/unified:repl-connected-p)
            (cl-tron-mcp/unified:repl-compile :code code
                                               :package (or package "CL-USER")
                                               :filename (or filename "repl"))
            (cl-tron-mcp/hot-reload:compile-and-load :code code :filename filename)))

(define-validated-tool "reload_system"
  "Reload an ASDF system in the connected Lisp image via Swank. Falls back to local reload when not connected."
  :input-schema (list :systemName "string" :force "boolean")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/reload-system.md"
  :validation ((validate-string "system_name" system_name :required t :min-length 1)
               (when force (validate-boolean "force" force)))
  :body (if (cl-tron-mcp/unified:repl-connected-p)
            (cl-tron-mcp/unified:repl-eval
             :code (format nil "(asdf:load-system ~s~a)"
                           system_name (if force " :force :all" "")))
            (cl-tron-mcp/hot-reload:reload-system :system-name system_name :force force)))