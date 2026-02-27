;;;; src/tools/xref-tools.lisp
;;;; Cross-reference tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "who_calls"
  "Find functions that call the given symbol. Use to understand dependencies before modifying a function."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-calls :symbol-name symbol_name))

(define-validated-tool "who_references"
  "Find references to the given symbol (variable references). Use to see where a variable is used."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-references :symbol-name symbol_name))

(define-validated-tool "who_binds"
  "Find bindings of the given symbol (let bindings, function parameters). Use to see where a variable is bound."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-binds :symbol-name symbol_name))

(define-validated-tool "who_sets"
  "Find setq/makunbound of the given symbol. Use to see where a variable is modified."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:who-sets :symbol-name symbol_name))

(define-validated-tool "list_callees"
  "List functions called by the given symbol"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/xref:list-callees :symbol-name symbol_name))