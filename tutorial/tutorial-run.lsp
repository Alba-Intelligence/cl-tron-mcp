#|
CL-TRON-MCP Tutorial Run

This tutorial demonstrates the CL-TRON-MCP debugging tools.
Usage: (load "tutorial/tutorial-run.lisp")
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-tron-mcp :silent t))

(format t "~%=== CL-TRON-MCP Tutorial Run ===~%~%")

(in-package :cl-user)

(format t "[0] Defining factorial function...~%")
(defpackage :tut (:use :cl))
(in-package :tut)
(defun factorial (n) (if (plusp n) (* n (factorial (1- n))) 1))
(format t "   factorial(5) = ~d~%" (factorial 5))

(in-package :cl-user)

(format t "~%[1] INSPECT-FUNCTION~%")
(format t "~a~%" (cl-tron-mcp:inspect-function "tut::factorial"))

(format t "~%[2] TRACE FUNCTION~%")
(cl-tron-mcp:trace-function "tut::factorial")
(format t "~a~%" (cl-tron-mcp:trace-list))
(cl-tron-mcp:trace-remove "tut::factorial")

(format t "~%[3] WHO-CALLS / LIST-CALLEES~%")
(format t "~a~%" (cl-tron-mcp:who-calls "tut::factorial"))
(format t "~a~%" (cl-tron-mcp:list-callees "tut::factorial"))

(format t "~%[4] DEBUGGER FRAMES~%")
(format t "~a~%" (cl-tron-mcp:debugger-frames :end 3))

(format t "~%[5] REPL EVAL~%")
(format t "~a~%" (cl-tron-mcp:repl-eval :code "(+ 10 20 30)" :package "CL-USER"))

(format t "~%[6] LOGGING~%")
(cl-tron-mcp:log-configure :level :info :package "cl-user")
(cl-tron-mcp:log-info :message "Tutorial test log" :package "cl-user")
(format t "   Logged message to cl-user package~%")

(format t "~%[7] SYSTEM INFO~%")
(let ((r (cl-tron-mcp:system-info)))
  (format t "   Lisp: ~a~%" (getf r :lisp-implementation))
  (format t "   Threads: ~a~%" (getf r :threads-count))
  (format t "   Packages: ~a~%" (getf r :packages-count)))

(format t "~%[8] RUNTIME STATS~%")
(let ((r (cl-tron-mcp:runtime-stats)))
  (format t "   Thread count: ~a~%" (getf r :thread-count)))

(format t "~%[9] HEALTH CHECK~%")
(format t "~a~%" (cl-tron-mcp:health-check))

(format t "~%[10] WHITELIST~%")
(cl-tron-mcp:whitelist-add :operation :eval :pattern "test*")
(format t "~a~%" (cl-tron-mcp:whitelist-status))

(format t "~%=== TUTORIAL COMPLETE ===~%")
