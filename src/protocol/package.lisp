;;;; src/protocol/package.lisp

(defpackage :cl-tron-mcp/protocol
  (:use :cl)
  (:import-from :cl-tron-mcp/util #:get-unix-time)
  (:import-from :cl-tron-mcp/tools
                #:validate-string-param
                #:validate-integer-param
                #:validate-boolean-param
                #:make-error-response
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
