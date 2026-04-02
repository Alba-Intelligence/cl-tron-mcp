;;;; src/tools/macros.lisp
;;;; Tool registration macros for reducing boilerplate

(in-package :cl-tron-mcp/tools)

(defmacro define-validated-tool (name description &key input-schema output-schema requires-approval documentation-uri validation body)
  "Define a tool with validation wrapper.
   VALIDATION is a list of validation forms.
   BODY is the actual implementation.

   Lambda parameters are derived from BOTH input-schema keys AND validation forms,
   so that body can reference any declared input parameter even if it has no validation."
  (let* ((schema-params (when input-schema
                          (loop for (k v) on (eval input-schema) by #'cddr
                                collect (intern (string-upcase (symbol-name k))))))
         (validation-params (when validation
                              (loop for (func param . args) in validation
                                    collect (intern (string-upcase param)))))
         (params (remove-duplicates (append schema-params validation-params))))
    `(progn
       (register-tool ,name ,description
                      :input-schema ,input-schema
                      :output-schema ,output-schema
                      :requires-approval ,requires-approval
                      :documentation-uri ,documentation-uri)
       (register-tool-handler ,name
                              (lambda (&key ,@params)
                                (handler-case
                                    (progn
                                      ,@(loop for (func param . args) in validation
                                              collect `(,func ,(string-downcase param) ,@args))
                                      ,body)
                                  (validation-error (e)
                                    (list :error t
                                          :message (format nil "Validation error: ~a" (validation-error-message e))
                                          :parameter (validation-error-parameter e)))))))))

(defmacro define-simple-tool (name description &key input-schema output-schema requires-approval documentation-uri function)
  "Define a tool that directly calls a function without validation."
  `(progn
     (register-tool ,name ,description
                    :input-schema ,input-schema
                    :output-schema ,output-schema
                    :requires-approval ,requires-approval
                    :documentation-uri ,documentation-uri)
     (register-tool-handler ,name (function ,function))))
