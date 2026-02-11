;;;; src/inspector/package.lisp

(defpackage :cl-tron-mcp/inspector
  (:use :cl)
  (:export
   #:inspect-object
   #:inspect-slot
   #:inspect-class
   #:inspect-function
   #:inspect-package))
