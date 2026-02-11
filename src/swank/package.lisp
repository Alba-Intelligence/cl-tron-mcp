;;;; src/swank/package.lisp

(defpackage :cl-tron-mcp/swank
  (:use :cl)
  (:export
   ;; Connection
   #:connect-swank
   #:disconnect-swank
   #:swank-connected-p
   ;; Evaluation
   #:swank-eval
   #:swank-compile
   ;; Debugging
   #:swank-threads
   #:swank-abort-thread
   #:swank-interrupt
   #:swank-backtrace
   #:swank-frame-locals
   ;; Inspector
   #:swank-inspect
   #:swank-describe
   ;; REPL
   #:swank-switch-to-repl
   #:swank-clear-repl
   ;; Autodoc
   #:swank-autodoc
   ;; Completion
   #:swank-completions
   ;; MCP Tools
   #:swank-connect
   #:swank-disconnect
   #:swank-status
   #:mcp-swank-eval
   #:mcp-swank-compile
   #:mcp-swank-threads
   #:mcp-swank-abort
   #:mcp-swank-interrupt
   #:mcp-swank-backtrace
   #:mcp-swank-frame-locals
   #:mcp-swank-inspect
   #:mcp-swank-describe
   #:mcp-swank-autodoc
   #:mcp-swank-completions
   #:swank-help))
