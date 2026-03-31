;;;; src/tools/inspector-tools.lisp
;;;; Inspector tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "inspect_object"
  "Deeply inspect a runtime object by registry ID"
  :input-schema (list :objectId "string" :maxDepth "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-object.md"
  :validation ((validate-object-id "object_id" object_id :required t)
               (when max_depth (validate-integer "max_depth" max_depth :min 0 :max 100)))
  :body (cl-tron-mcp/inspector:inspect-object :object-id object_id :max-depth max_depth))

(define-validated-tool "inspect_slot"
  "Read or write a named slot on an inspected object"
  :input-schema (list :objectId "string" :slotName "string" :value "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-slot.md"
  :validation ((validate-object-id "object_id" object_id :required t)
               (validate-string "slot_name" slot_name :required t :min-length 1)
               (when value (validate-string "value" value)))
  :body (cl-tron-mcp/inspector:inspect-slot :object-id object_id :slot-name slot_name :value value))

(define-validated-tool "inspect_class"
  "Inspect a CLOS class: slots, superclasses, and methods"
  :input-schema (list :className "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-class.md"
  :validation ((validate-string "class_name" class_name :required t :min-length 1))
  :body (cl-tron-mcp/inspector:inspect-class :class-name class_name))

(define-validated-tool "inspect_function"
  "Inspect a function: arglist, source, type, and documentation"
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-function.md"
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/inspector:inspect-function :symbol_name symbol_name))

(define-validated-tool "inspect_package"
  "List all exported and internal symbols in a package"
  :input-schema (list :packageName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-package.md"
  :validation ((validate-package-name "package_name" package_name :required t))
  :body (cl-tron-mcp/inspector:inspect-package :package-name package_name))