;;;; src/protocol/package.lisp

(defpackage :cl-tron-mcp/protocol
  (:use :cl)
  (:import-from :cl-tron-mcp/tools
                #:handle-tools-list
                #:handle-tool-call
                #:handle-approval-respond)
  (:export
    #:handle-resources-list
    #:handle-resources-read
    #:handle-message
    #:handle-request
    #:handle-notification
    #:handle-initialize
    #:*message-handler*
    #:*request-id*
    #:make-response
    #:make-error-response
    #:parse-message))
