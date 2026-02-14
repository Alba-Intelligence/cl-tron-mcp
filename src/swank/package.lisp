;;;; src/swank/package.lisp

(defpackage :cl-tron-mcp/swank
  (:use :cl)
  (:nicknames :cl-tron-mcp/swank/client)
  (:shadowing-import-from :cl-tron-mcp/logging
    #:log-info
    #:log-debug
    #:log-warn
    #:log-error)
  (:export
   ;; Connection
   #:swank-connect
   #:swank-disconnect
   #:swank-status
   #:swank-connected-p
   ;; Evaluation
   #:swank-eval
   #:mcp-swank-eval
   #:swank-compile
   #:mcp-swank-compile
   ;; Backtrace/Debugging
   #:swank-backtrace
   #:mcp-swank-backtrace
   #:swank-frame-locals
   #:mcp-swank-frame-locals
   #:swank-eval-in-frame
   #:swank-get-restarts
   #:swank-invoke-restart
   ;; Stepping
   #:swank-step
   #:swank-next
   #:swank-out
   #:swank-continue
   #:swank-debugger-state
   ;; Breakpoints
   #:swank-set-breakpoint
   #:mcp-swank-set-breakpoint
   #:swank-remove-breakpoint
   #:mcp-swank-remove-breakpoint
   #:swank-list-breakpoints
   #:mcp-swank-list-breakpoints
   #:swank-toggle-breakpoint
   #:mcp-swank-toggle-breakpoint
   ;; Threads
   #:swank-threads
   #:mcp-swank-threads
   #:swank-abort-thread
   #:mcp-swank-abort
   #:swank-interrupt
   #:mcp-swank-interrupt
   ;; Inspector
   #:swank-inspect-object
   #:mcp-swank-inspect
   #:swank-inspect-nth-part
   ;; Description/Documentation
   #:swank-describe
   #:mcp-swank-describe
   #:swank-autodoc
   #:mcp-swank-autodoc
   #:swank-completions
   #:mcp-swank-completions
   ;; Events
   #:pop-debugger-event
   #:swank-event-processor
   #:handle-swank-event))
