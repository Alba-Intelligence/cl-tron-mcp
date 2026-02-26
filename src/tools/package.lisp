;;;; src/tools/package.lisp

(defpackage :cl-tron-mcp/tools
  (:use :cl)
  (:export
   #:*tool-registry*
   #:define-tool
   #:register-tool
   #:list-tool-descriptors
   #:call-tool
   #:get-tool-handler
   #:get-tool-descriptor
   #:tool-requires-user-approval-p
   #:validation-error
   #:validation-error-parameter
   #:validation-error-message
   #:validate-required
   #:validate-string
   #:validate-integer
   #:validate-boolean
   #:validate-choice
   #:validate-object-id
   #:validate-symbol-name
   #:validate-package-name
   #:with-validation))
