;;;; src/tracer/package.lisp

(defpackage :cl-tron-mcp/tracer
  (:use :cl)
  (:export
   #:trace-function
   #:trace-remove
   #:trace-list
   #:trace-clear
   #:trace-get-entries))
