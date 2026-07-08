;;;; src/tools/package.lisp
;;;; Define package for all tools

(defpackage :cl-tron-mcp/tools
  (:use :cl)
  (:import-from :cl-tron-mcp/config
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
  
  (:export
    #:handle-tools-list
    #:handle-tool-call
    #:define-simple-tool
    #:define-validated-tool
    #:register-tool-handler
    #:list-tool-descriptors
    #:repl-help
   
   ;; Connection
   #:repl-connect
   #:repl-disconnect
   #:repl-connected-p
   #:repl-status
   #:repl-type

   #:*repl-connected*
   #:*repl-type*
   #:*repl-port*
   #:*repl-host*

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
   #:repl-toggle-breakpoint))
