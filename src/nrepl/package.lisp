;;;; src/nrepl/package.lisp - nrepl client package

(in-package :cl)

(defpackage :cl-tron-mcp/nrepl
  (:use :cl)
  (:export
   ;; Connection
   #:nrepl-connect
   #:nrepl-disconnect
   #:nrepl-status
   #:nrepl-connected-p

   ;; Evaluation
   #:nrepl-eval
   #:nrepl-compile

   ;; Session
   #:nrepl-sessions
   #:nrepl-close-session

   ;; Operations
   #:nrepl-threads
   #:nrepl-interrupt
   #:nrepl-backtrace
   #:nrepl-inspect
   #:nrepl-describe
   #:nrepl-completions
   #:nrepl-doc

   ;; Help
   #:nrepl-help))
