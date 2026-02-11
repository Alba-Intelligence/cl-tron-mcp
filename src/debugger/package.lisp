;;;; src/debugger/package.lisp

(defpackage :cl-tron-mcp/debugger
  (:use :cl)
  (:export
   ;; Frames
   #:get-debugger-frames
   #:get-frame-locals
   #:eval-in-frame
   ;; Restarts
   #:list-restarts
   #:invoke-restart
   ;; Breakpoints
   #:set-breakpoint
   #:remove-breakpoint
   #:list-breakpoints
   #:toggle-breakpoint
   #:get-breakpoint-info
   ;; Stepping
   #:step-frame
   #:step-into
   #:step-over
   #:step-out
   #:continue-execution
   #:get-stepping-state))
