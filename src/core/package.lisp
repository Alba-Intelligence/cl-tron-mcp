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
   #:set-config
   #:*config-file-paths*
   #:*config-loaded*
   #:*env-var-prefix*
   #:load-config-file
   #:load-config-from-env
   #:load-configuration
   #:get-config-value
   #:print-configuration
   #:*token-stats*
   #:*token-tracking-enabled*
   #:*token-stats-lock*
   #:count-tokens
   #:count-response-tokens
   #:track-response
   #:get-token-stats
   #:reset-token-stats
   #:generate-token-report
#:with-token-tracking
    #:benchmark-tool
    #:*error-codes*
    #:get-error-info
    #:get-error-message
    #:get-error-hint
    #:get-error-setup
    #:get-error-documentation-uri
    #:make-error
    #:make-error-with-hint
    #:make-error-full
    #:make-not-connected-error
    #:make-already-connected-error))
