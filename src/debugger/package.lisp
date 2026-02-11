;;;; src/debugger/package.lisp

(defpackage :cl-tron-mcp/debugger
  (:use :cl)
  (:export
   #:get-debugger-frames
   #:get-frame-locals
   #:eval-in-frame
   #:list-restarts
   #:invoke-restart
   #:set-breakpoint
   #:remove-breakpoint
   #:list-breakpoints
   #:step-frame))
