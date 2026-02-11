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
   #:get-tool-descriptor
   #:get-tool-handler
   #:get-config
   #:set-config))
