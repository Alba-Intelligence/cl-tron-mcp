;;;; src/tracer/package.lisp

(defpackage :cl-tron-mcp/tracer
  (:use :cl)
  (:export
    #:trace-function
    #:trace-remove
    #:trace-list
    #:trace-clear
    #:trace-get-entries
    #:trace-capture-entry
    ;; Metrics
    #:*metrics-enabled*
    #:metrics-record-call
    #:with-metrics
    #:metrics-snapshot
    #:metrics-reset
    #:metrics-for-tool))
