;;;; src/core/package.lisp

(defpackage :cl-tron-mcp/core
  (:use :cl)
  (:export
   #:*version*
   #:*config*
   #:*server-state*
   #:*audit-log*
   #:start-server
   #:stop-server
   #:*current-transport*
   #:get-server-state
   #:get-transport-type
   #:get-tool-descriptor
   #:get-tool-handler
   #:get-config
   #:set-config))
