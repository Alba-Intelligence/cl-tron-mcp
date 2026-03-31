;;;; src/sbcl/package.lisp

(defpackage :cl-tron-mcp/sbcl
  (:use :cl)
   (:export
    ;; Safe evaluation
    #:safe-eval
    #:safe-compile
    #:eval-with-output
    ;; Thread management
    #:find-thread
    #:list-threads
    #:get-thread-state
    #:thread-state
    #:inspect-thread
    #:thread-backtrace
    ;; Object registry
    #:get-object-id
    #:lookup-object
    #:register-object
    #:clear-object-registry
    #:evict-expired-objects
    #:evict-lru-objects
    #:maybe-evict-objects
    #:*object-registry*
    #:*object-registry-max-age*
    #:*object-registry-max-size*))
