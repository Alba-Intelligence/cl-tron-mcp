;;;; src/unified/package.lisp - Unified REPL interface package

(in-package :cl)

(defpackage :cl-tron-mcp/unified
  (:use :cl)
  (:import-from :cl-tron-mcp/core
                #:*version*)
  (:import-from :cl-tron-mcp/swank
                #:swank-connect
                #:swank-disconnect
                #:swank-status
                #:mcp-swank-eval
                #:mcp-swank-compile
                #:mcp-swank-threads
                #:mcp-swank-abort
                #:mcp-swank-backtrace
                #:mcp-swank-frame-locals
                #:mcp-swank-inspect
                #:mcp-swank-describe
                #:mcp-swank-completions
                #:mcp-swank-autodoc
                #:swank-step
                #:swank-next
                #:swank-out
                #:swank-continue
                #:swank-get-restarts
                #:swank-invoke-restart
                #:mcp-swank-set-breakpoint
                #:mcp-swank-remove-breakpoint
                #:mcp-swank-list-breakpoints
                #:mcp-swank-toggle-breakpoint)
  (:import-from :cl-tron-mcp/nrepl
                #:nrepl-connect
                #:nrepl-disconnect
                #:nrepl-status
                #:nrepl-eval
                #:nrepl-compile
                #:nrepl-threads
                #:nrepl-backtrace
                #:nrepl-inspect
                #:nrepl-describe
                #:nrepl-completions
                #:nrepl-doc)
  (:export
   ;; Connection
   #:repl-connect
   #:repl-disconnect
   #:repl-status
   #:repl-type

   ;; Evaluation
   #:repl-eval
   #:repl-compile

   ;; Operations
   #:repl-threads
   #:repl-abort
   #:repl-backtrace
   #:repl-frame-locals
   #:repl-inspect
   #:repl-describe
   #:repl-completions
   #:repl-doc

   ;; Debugger
   #:repl-step
   #:repl-next
   #:repl-out
   #:repl-continue
   #:repl-get-restarts
   #:repl-invoke-restart

   ;; Breakpoints
   #:repl-set-breakpoint
   #:repl-remove-breakpoint
   #:repl-list-breakpoints
   #:repl-toggle-breakpoint

   ;; Help
   #:repl-help))
