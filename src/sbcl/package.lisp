;;;; src/sbcl/package.lisp

(defpackage :cl-tron-mcp/sbcl
  (:use :cl)
  (:export
   #:safe-eval
   #:safe-compile
   #:find-thread
   #:list-threads
   #:get-thread-state
   #:get-object-id
   #:lookup-object
   #:*object-registry*))
