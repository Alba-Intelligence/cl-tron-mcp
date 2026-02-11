;;;; src/protocol/package.lisp

(defpackage :cl-tron-mcp/protocol
  (:use :cl)
  (:export
   #:handle-message
   #:handle-request
   #:handle-notification
   #:*message-handler*
   #:*request-id*
   #:make-response
   #:make-error-response
   #:parse-message))
