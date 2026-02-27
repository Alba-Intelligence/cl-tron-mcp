;;;; src/tools/inspector-tools.lisp
;;;; Inspector tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "inspect_object"
  "Inspect an object by its ID. Use when you need to examine the slots and structure of a CLOS instance or other object. Returns type, slots, and nested object IDs for further inspection."
  :input-schema (list :objectId "string" :maxDepth "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-object-id "object_id" object_id :required t)
               (when max_depth (validate-integer "max_depth" max_depth :min 0 :max 100)))
  :body (cl-tron-mcp/inspector:inspect-object :object-id object_id :max-depth max_depth))

(define-validated-tool "inspect_slot"
  "Get or set a slot value on an object. Use to read or modify individual slots of a CLOS instance. Set value by providing the value parameter."
  :input-schema (list :objectId "string" :slotName "string" :value "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-object-id "object_id" object_id :required t)
               (validate-string "slot_name" slot_name :required t :min-length 1)
               (when value (validate-string "value" value)))
  :body (cl-tron-mcp/inspector:inspect-slot :object-id object_id :slot-name slot_name :value value))

(define-validated-tool "inspect_class"
  "Inspect a CLOS class definition. Shows superclasses, slots, and methods. Use to understand class structure before working with instances."
  :input-schema (list :className "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "class_name" class_name :required t :min-length 1))
  :body (cl-tron-mcp/inspector:inspect-class :class-name class_name))

(define-validated-tool "inspect_function"
  "Inspect a function definition. Shows lambda list, documentation, and source location. Use to understand how a function should be called."
  :input-schema (list :symbolName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-symbol-name "symbol_name" symbol_name :required t))
  :body (cl-tron-mcp/inspector:inspect-function :symbol-name symbol_name))

(define-validated-tool "inspect_package"
  "Inspect a package and list its exported symbols. Use to discover available functions and variables in a package."
  :input-schema (list :packageName "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-package-name "package_name" package_name :required t))
  :body (cl-tron-mcp/inspector:inspect-package :package-name package_name))