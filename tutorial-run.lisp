;;;; tutorial-run.lisp - Run the CL-TRON-MCP tutorial

(format t "~%=== CL-TRON-MCP Tutorial Run ===~%~%")
(format t "Loading cl-tron-mcp...~%")
(ql:quickload :cl-tron-mcp :silent t)
(format t "Loaded!~%~%")

;; Define function
(defpackage :tut (:use :cl))
(in-package :tut)
(defun factorial (n) (if (plusp n) (* n (factorial (1- n))) 1))
(format t "[0] Defined factorial(5)=~d~%" (factorial 5))
;; Export so inspector can find it
(export 'factorial :tut)

;; Use the main package for tools
(in-package :cl-user)

;; Inspector
(format t "~%[1] INSPECT-FUNCTION (CL:CAR)~%")
(format t "~a~%" (cl-tron-mcp/inspector:inspect-function "cl:car"))

;; Tracer
(format t "~%[2] TRACE FUNCTION~%")
(cl-tron-mcp/tracer:trace-function "tut::factorial")
(format t "~a~%" (cl-tron-mcp/tracer:trace-list))
(cl-tron-mcp/tracer:trace-remove "tut::factorial")

;; Use a simpler function for XRef (built-in)
(format t "~%[3] WHO-CALLS (FIND-PACKAGE)~%")
(format t "~a~%" (cl-tron-mcp/xref:who-calls "cl:find-package"))

;; Debugger
(format t "~%[4] DEBUGGER FRAMES~%")
(format t "~a~%" (cl-tron-mcp/debugger:get-debugger-frames :end 3))

;; REPL
(format t "~%[5] REPL EVAL~%")
(format t "~a~%" (cl-tron-mcp/repl:repl-eval "(+ 10 20 30)" :package :cl-user))

;; Logging
(format t "~%[6] LOGGING~%")
(cl-tron-mcp/logging:log-configure :level :info :package "CL-USER")
(cl-tron-mcp/logging:log-info "Tutorial test log" :package "CL-USER")
(format t "   Logged message~%")

;; System Info
(format t "~%[7] SYSTEM INFO~%")
(let ((r (cl-tron-mcp/monitor:system-info)))
  (format t "   Lisp: ~a~%" (getf r :lisp-implementation))
  (format t "   Threads: ~a~%" (getf r :threads-count))
  (format t "   Packages: ~a~%" (getf r :packages-count)))

;; Runtime Stats
(format t "~%[8] RUNTIME STATS~%")
(let ((r (cl-tron-mcp/monitor:runtime-stats)))
  (format t "   Thread count: ~a~%" (getf r :thread-count)))

;; Health Check
(format t "~%[9] HEALTH CHECK~%")
(format t "~a~%" (cl-tron-mcp/monitor:health-check))

;; Security - Approval
(format t "~%[10] APPROVAL CHECK~%")
(format t "   Eval requires approval: ~a~%" (cl-tron-mcp/security:operation-requires-approval :eval))
(format t "   Unknown op requires approval: ~a~%" (cl-tron-mcp/security:operation-requires-approval :unknown))

(format t "~%=== TUTORIAL COMPLETE ===~%")
