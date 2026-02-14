;;;; src/debugger/package.lisp

(defpackage :cl-tron-mcp/debugger
  (:use :cl)
  (:import-from :cl-tron-mcp/swank
                #:swank-get-restarts
                #:swank-invoke-restart
                #:swank-backtrace
                #:swank-frame-locals
                #:swank-eval-in-frame
                #:swank-step
                #:swank-next
                #:swank-out
                #:swank-continue
                #:swank-set-breakpoint
                #:swank-remove-breakpoint
                #:swank-list-breakpoints
                #:swank-toggle-breakpoint)
  (:export
   ;; Frames
   #:get-debugger-frames
   #:get-frame-locals
   #:eval-in-frame
   ;; Restarts
   #:list-restarts
   #:invoke-named-restart
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
