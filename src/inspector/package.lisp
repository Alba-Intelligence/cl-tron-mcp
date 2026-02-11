;;;; src/inspector/package.lisp

(defpackage :cl-tron-mcp/inspector
  (:use :cl)
  (:import-from :closer-mop
   #:class-direct-superclasses
   #:class-direct-slots
   #:class-precedence-list
   #:slot-definition-name)
  (:export
   #:inspect-object
   #:inspect-slot
   #:inspect-class
   #:inspect-function
   #:inspect-package))
