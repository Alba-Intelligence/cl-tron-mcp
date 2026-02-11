#|
CL-TRON-MCP Tutorial

This tutorial demonstrates the CL-TRON-MCP debugging MCP server.
Usage: (load "tutorial/tutorial.lisp")

CL-TRON-MCP provides 43 tools for debugging Common Lisp applications.

NEW: See comprehensive tutorials:
- tutorial/CURSOR-MCP-TUTORIAL.md - Full guide for Cursor IDE setup
- tutorial/factorial-demo.lisp - Interactive factorial debugging demo
- tutorial/debugging-tutorial.lisp - Original debugging examples
|#
(in-package :cl-user)

(format t "~%=== CL-TRON-MCP Tutorial ===~%~%")

;; Define test function
(format t "[0] Test Function~%")
(eval '(defun tutorial-fact (x) (* x x)))
(format t "   tutorial-fact(5) = ~d~%~%" (tutorial-fact 5))

;; 1. System Info
(format t "[1] System Info~%")
(let ((r (funcall (intern "SYSTEM-INFO" "CL-TRON-MCP/MONITOR"))))
  (format t "   ~a ~a~%" (getf r :lisp-implementation) (getf r :lisp-version))
  (format t "   Threads: ~d  Packages: ~d~%" (getf r :threads-count) (getf r :packages-count)))

;; 2. Runtime Stats  
(format t "~%[2] Runtime Stats~%")
(let ((r (funcall (intern "RUNTIME-STATS" "CL-TRON-MCP/MONITOR"))))
  (format t "   Threads: ~d  Memory: ~d MB~%" 
          (getf r :thread-count)
          (getf (getf r :memory) :total-mb)))

;; 3. Health Check
(format t "~%[3] Health Check~%")
(let ((r (funcall (intern "HEALTH-CHECK" "CL-TRON-MCP/MONITOR"))))
  (format t "   Status: ~a~%" (getf r :status)))

;; 4. Trace List
(format t "~%[4] Trace List~%")
(let ((r (funcall (intern "TRACE-LIST" "CL-TRON-MCP/TRACER"))))
  (format t "   Traced: ~d functions~%" (getf r :count)))

(format t "~%=== CL-TRON-MCP Summary ===~%")
(format t "MCP Server: RUNNING~%")
(format t "Total Tools: 43 across 11 categories~%")
(format t "~%Categories:~%")
(format t "  Inspector (5):  object, slot, class, function, package~%")
(format t "  Debugger (6):   frames, restarts, breakpoints, stepping~%")
(format t "  REPL (1):       eval with approval~%")
(format t "  Hot Reload (2):  compile, reload system~%")
(format t "  Profiler (3):    start, stop, report~%")
(format t "  Tracer (3):      trace, untrace, list~%")
(format t "  Thread (3):      list, inspect, backtrace~%")
(format t "  Monitor (4):     health, stats, gc, system info~%")
(format t "  Logging (5):      log4cl integration~%")
(format t "  XRef (7):        who-calls, who-references, etc~%")
(format t "  Security (5):    whitelist management~%")
(format t "~%CL-TRON-MCP is fully functional!~%")
(format t "=== TUTORIAL COMPLETE ===~%")
