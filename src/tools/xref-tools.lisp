;;;; src/tools/xref-tools.lisp
;;;; Cross-reference tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "who_calls"
  "Find all functions that call the named function"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/who-calls.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-calls :symbol-name symbol_name))

(define-validated-tool "who_references"
  "Find all code locations that reference a global variable"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/who-references.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-references :symbol-name symbol_name))

(define-validated-tool "who_binds"
  "Find all code locations that bind a variable with let or similar"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/who-binds.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-binds :symbol-name symbol_name))

(define-validated-tool "who_sets"
  "Find all code locations that assign to a global variable"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/who-sets.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-sets :symbol-name symbol_name))

(define-validated-tool "list_callees"
  "List all functions called by the named function"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/list-callees.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:list-callees :symbol-name symbol_name))