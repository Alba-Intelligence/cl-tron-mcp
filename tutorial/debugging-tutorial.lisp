;;;; CL-TRON-MCP Tutorial: Debugging a Real-World Scenario
;;;;
;;;; Demonstrates using CL-TRON-MCP tools to debug a buggy factorial function.
;;;; All #{...}# blocks from the original have been replaced with either:
;;;;   - real runnable Lisp calls (direct package-qualified API), or
;;;;   - #| block comments |# showing the equivalent MCP tool JSON.

;;; ============================================================
;;; STEP 1: Bootstrap — load CL-TRON-MCP and connect to Swank
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-tron-mcp :silent t))

;; Launch a managed Swank process on port 14006 (self-contained):
(let ((port 14006))
  (cl-tron-mcp/swank:launch-sbcl-with-swank :port port)
  (cl-tron-mcp/swank:wait-for-port port :timeout 30)
  (cl-tron-mcp/unified:repl-connect :port port))

;;; ============================================================
;;; STEP 2: Define a buggy function in the connected image
;;; ============================================================

(defpackage :tutorial (:use :cl))
(in-package :tutorial)

(defun factorial (n)
  "Calculate factorial of N. This has a subtle bug with negative numbers!"
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

;; Test it (works for non-negative):
(format t "factorial(5) = ~d~%" (factorial 5))  ; => 120
(format t "factorial(0) = ~d~%" (factorial 0))  ; => 1

(in-package :cl-user)

;; Now define it in the target Swank image via repl-compile:
(cl-tron-mcp/unified:repl-compile
  :code "(defpackage :tutorial (:use :cl))
(in-package :tutorial)
(defun factorial (n)
  (if (plusp n) (* n (factorial (1- n))) 1))")

;;; ============================================================
;;; STEP 3: Inspector — examine the function
;;; ============================================================

;; Direct Lisp call:
(format t "~%[inspect_function]~%~a~%"
        (cl-tron-mcp/inspector:inspect-function :symbol_name "tutorial::factorial"))

#|
Equivalent MCP tool JSON:
{
  "tool": "inspect_function",
  "arguments": { "functionName": "tutorial::factorial" }
}
|#

;;; ============================================================
;;; STEP 4: Tracer — track function calls
;;; ============================================================

;; Trace the function:
(format t "~%[trace_function]~%~a~%"
        (cl-tron-mcp/tracer:trace-function "tutorial::factorial"))

;; Check what's being traced:
(format t "~%[trace_list]~%~a~%"
        (cl-tron-mcp/tracer:trace-list))

;; Evaluate through the tracer to see call tree:
(cl-tron-mcp/unified:repl-eval :code "(tutorial::factorial 3)" :package "TUTORIAL")

;; Remove trace:
(cl-tron-mcp/tracer:trace-remove "tutorial::factorial")

#|
Equivalent MCP tool JSON:
{ "tool": "trace_function", "arguments": { "functionName": "tutorial::factorial" } }
{ "tool": "trace_list",     "arguments": {} }
{ "tool": "repl_eval",      "arguments": { "code": "(tutorial::factorial 3)" } }
{ "tool": "trace_remove",   "arguments": { "functionName": "tutorial::factorial" } }
|#

;;; ============================================================
;;; STEP 5: Cross-references
;;; ============================================================

(format t "~%[who_calls tutorial::factorial]~%~a~%"
        (cl-tron-mcp/xref:who-calls "tutorial::factorial"))

(format t "~%[list_callees tutorial::factorial]~%~a~%"
        (cl-tron-mcp/xref:list-callees "tutorial::factorial"))

#|
{ "tool": "who_calls",    "arguments": { "symbolName": "tutorial::factorial" } }
{ "tool": "list_callees", "arguments": { "symbolName": "tutorial::factorial" } }
|#

;;; ============================================================
;;; STEP 6: Debugger frames and restarts
;;; ============================================================

(format t "~%[debugger_frames]~%~a~%"
        (cl-tron-mcp/debugger:get-debugger-frames :start 0 :end 5))

(format t "~%[debugger_restarts]~%~a~%"
        (cl-tron-mcp/debugger:list-restarts))

#|
{ "tool": "debugger_frames",   "arguments": { "start": 0, "end": 10 } }
{ "tool": "debugger_restarts", "arguments": {} }
|#

;;; ============================================================
;;; STEP 7: REPL eval via unified interface
;;; ============================================================

(format t "~%[repl_eval]~%~a~%"
        (cl-tron-mcp/unified:repl-eval
          :code "(format nil \"Testing factorial: ~d\" (tutorial::factorial 5))"
          :package "CL-USER"))

#|
{ "tool": "repl_eval",
  "arguments": {
    "code": "(format nil \"Testing factorial: ~d\" (tutorial::factorial 5))",
    "package": "CL-USER"
  }
}
|#

;;; ============================================================
;;; STEP 8: Logging
;;; ============================================================

(cl-tron-mcp/logging:log-configure :level :debug :package "tutorial")
(cl-tron-mcp/logging:log-debug "Calculating factorial" :package "tutorial")
(format t "Logged debug message for tutorial package.~%")

#|
{ "tool": "log_configure", "arguments": { "level": "debug", "package": "tutorial" } }
{ "tool": "log_debug",     "arguments": { "message": "Calculating factorial", "package": "tutorial" } }
|#

;;; ============================================================
;;; STEP 9: Hot reload — fix the bug
;;; ============================================================

(format t "~%[code_compile_string — fix factorial]~%")
(cl-tron-mcp/unified:repl-compile
  :code "(in-package :tutorial)
(defun factorial (n)
  \"Factorial — now handles negative numbers correctly.\"
  (cond
    ((plusp n) (* n (factorial (1- n))))
    ((zerop n) 1)
    (t (error \"Factorial undefined for negative: ~d\" n))))")

(format t "Fixed factorial(5) = ~a~%"
        (cl-tron-mcp/unified:repl-eval :code "(tutorial::factorial 5)"))

#|
{
  "tool": "repl_compile",
  "arguments": {
    "code": "(in-package :tutorial)\n(defun factorial (n) ...)"
  }
}
|#

;;; ============================================================
;;; STEP 10: Monitoring and system info
;;; ============================================================

(format t "~%[system_info]~%~a~%" (cl-tron-mcp/monitor:system-info))
(format t "~%[runtime_stats]~%~a~%" (cl-tron-mcp/monitor:runtime-stats))
(format t "~%[health_check]~%~a~%" (cl-tron-mcp/monitor:health-check))

#|
{ "tool": "system_info",   "arguments": {} }
{ "tool": "runtime_stats", "arguments": {} }
{ "tool": "health_check",  "arguments": {} }
|#

;;; ============================================================
;;; STEP 11: Thread management
;;; ============================================================

(format t "~%[thread_list]~%~a~%"
        (cl-tron-mcp/sbcl:list-threads))

#|
{ "tool": "thread_list",    "arguments": {} }
{ "tool": "thread_inspect", "arguments": { "threadId": "main thread" } }
|#

;;; ============================================================
;;; STEP 12: Security whitelist
;;; ============================================================

(cl-tron-mcp/security:whitelist-add :eval "tutorial::factorial*")
(format t "~%[whitelist_status]~%~a~%" (cl-tron-mcp/security:whitelist-status))

#|
{ "tool": "whitelist_add",    "arguments": { "operation": "eval", "pattern": "tutorial::factorial*" } }
{ "tool": "whitelist_status", "arguments": {} }
|#

;;; ============================================================
;;; STEP 13: Profiling
;;; ============================================================

#|
{ "tool": "profile_start",  "arguments": {} }
... run computations ...
{ "tool": "profile_stop",   "arguments": {} }
{ "tool": "profile_report", "arguments": { "format": "flat" } }
|#

;;; ============================================================
;;; Cleanup — disconnect from Swank
;;; ============================================================

(cl-tron-mcp/unified:repl-disconnect)

;;; ============================================================
;;; Summary
;;; ============================================================

;; CL-TRON-MCP provides 91 tools across 14 categories:
;; - Object and function inspection (inspector_*)
;; - Function tracing (trace_*)
;; - Cross-reference analysis (who_calls, list_callees, ...)
;; - Interactive debugging via REPL (repl_eval, repl_compile)
;; - Hot code reloading (code_compile_string, reload_system)
;; - Thread management (thread_list, thread_backtrace)
;; - Performance profiling (profile_start, profile_stop)
;; - Logging integration (log_configure, log_info, ...)
;; - Process management (swank_launch, swank_kill, ...)
