#|
CL-TRON-MCP Tutorial Run

Demonstrates the CL-TRON-MCP tools using correct package-qualified API.
This script exercises LOCAL tools (inspector, tracer, xref, debugger, monitor, 
logging, security) which work on the current SBCL image — no Swank required.

For Swank-based repl_eval demos, see tutorial/tutorial.lisp or the f1-f2 tutorial.

Usage: sbcl --noinform --disable-debugger \
            --eval '(ql:quickload :cl-tron-mcp :silent t)' \
            --load tutorial/tutorial-run.lsp \
            --eval '(sb-ext:quit)'
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-tron-mcp :silent t))

(format t "~%=== CL-TRON-MCP Tutorial Run ===~%~%")

(in-package :cl-user)

;;; ---- Define a test function locally ----

(format t "[0] Defining factorial function locally...~%")
(defpackage :tut (:use :cl))
(in-package :tut)
(defun factorial (n) (if (plusp n) (* n (factorial (1- n))) 1))
(format t "   factorial(5) = ~d~%" (factorial 5))

(in-package :cl-user)

;;; ---- Demonstrate each tool category ----

(format t "~%[1] INSPECT-FUNCTION~%")
(format t "~a~%" (cl-tron-mcp/inspector:inspect-function :symbol_name "tut::factorial"))

(format t "~%[2] TRACE FUNCTION~%")
(cl-tron-mcp/tracer:trace-function "tut::factorial")
(format t "~a~%" (cl-tron-mcp/tracer:trace-list))
(cl-tron-mcp/tracer:trace-remove "tut::factorial")

(format t "~%[3] WHO-CALLS / LIST-CALLEES~%")
(format t "~a~%" (cl-tron-mcp/xref:who-calls "tut::factorial"))
(format t "~a~%" (cl-tron-mcp/xref:list-callees "tut::factorial"))

(format t "~%[4] DEBUGGER FRAMES~%")
(format t "~a~%" (cl-tron-mcp/debugger:get-debugger-frames :start 0 :end 3))

(format t "~%[5] LOGGING~%")
(cl-tron-mcp/logging:log-configure :level :info :package "cl-user")
(cl-tron-mcp/logging:log-info "Tutorial test log" :package "cl-user")
(format t "   Logged info message to cl-user package~%")

(format t "~%[6] SYSTEM INFO~%")
(let ((r (cl-tron-mcp/monitor:system-info)))
  (format t "   Lisp: ~a~%" (getf r :lisp-implementation))
  (format t "   Threads: ~a~%" (getf r :threads-count))
  (format t "   Packages: ~a~%" (getf r :packages-count)))

(format t "~%[7] RUNTIME STATS~%")
(let ((r (cl-tron-mcp/monitor:runtime-stats)))
  (format t "   Thread count: ~a~%" (getf r :thread-count)))

(format t "~%[8] HEALTH CHECK~%")
(format t "~a~%" (cl-tron-mcp/monitor:health-check))

(format t "~%[9] WHITELIST~%")
(cl-tron-mcp/security:whitelist-add :eval "test*")
(format t "~a~%" (cl-tron-mcp/security:whitelist-status))

(format t "~%[10] TOOL REGISTRY~%")
(format t "   Registered tools: ~d~%"
        (hash-table-count cl-tron-mcp/tools:*tool-registry*))

(format t "~%=== TUTORIAL COMPLETE ===~%")
