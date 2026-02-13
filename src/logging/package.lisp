;;;; src/logging/package.lisp

(defpackage :cl-tron-mcp/logging
  (:use :cl)
  (:export
   #:log-configure
   #:log-level
   #:log-info
   #:log-debug
   #:log-warn
   #:log-error
   #:get-log-config
   #:ensure-log-to-stream))
