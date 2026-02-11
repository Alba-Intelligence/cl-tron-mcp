;;;; src/swank/package.lisp

(defpackage :cl-tron-mcp/swank
  (:use :cl)
  (:export
   ;; Connection
   #:connect-swank
   #:disconnect-swank
   #:swank-connected-p
   ;; Internal message passing (for testing)
   #:send-swank-message
   ;; MCP Wrappers
   #:swank-connect
   #:swank-disconnect
   #:swank-status
   ;; Evaluation
   #:swank-eval-internal
   #:mcp-swank-eval
   #:swank-compile-internal
   #:mcp-swank-compile
   ;; Threads
   #:mcp-swank-threads
   #:mcp-swank-abort
   #:mcp-swank-interrupt
   ;; Debugging
   #:mcp-swank-backtrace
   #:mcp-swank-frame-locals
   ;; Inspector
   #:mcp-swank-inspect
   #:mcp-swank-describe
   ;; Documentation
   #:mcp-swank-autodoc
   #:mcp-swank-completions
   ;; Help
   #:swank-help))
