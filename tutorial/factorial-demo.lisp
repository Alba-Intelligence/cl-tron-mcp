#|
CL-TRON-MCP Interactive Tutorial: Factorial Debugging Example

This tutorial replicates the Common Lisp Cookbook debugging examples:
https://lispcookbook.github.io/cl-cookbook/debugging.html

Usage: (load "tutorial/factorial-demo.lisp")

This demonstrates how an AI Agent can interact with Lisp sessions
like a user at a keyboard, including tracing, breakpoints, and
debugger integration.
|#
(in-package :cl-user)

(format t "~%========================================~%")
(format t "  CL-TRON-MCP Interactive Debugging Tutorial~%")
(format t "  Factorial Function - CL Cookbook Example~%")
(format t "========================================~%~%")

;; Step 1: Define the factorial function
(format t "[STEP 1] Defining factorial function...~%")
(format t "~%")

(defun factorial (n)
  "Compute the factorial of N recursively.
   This is the classic example from the CL Cookbook."
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

(format t "   (defun factorial (n)~%")
(format t "     (if (plusp n)~%")
(format t "         (* n (factorial (1- n)))~%")
(format t "         1))~%")
(format t "~%   factorial defined: ~a~%" (documentation 'factorial 'function))
(format t "~%")

;; Step 2: Test the function
(format t "[STEP 2] Testing factorial(5)...~%")
(let ((result (factorial 5)))
  (format t "   (factorial 5) => ~d~%" result))
(format t "~%")

;; Step 3: Explain what we'll do with CL-TRON-MCP
(format t "[STEP 3] CL-TRON-MCP AI Agent Commands~%")
(format t "~%")
(format t "   The AI agent can now trace, debug, and inspect this function.~%")
(format t "~%")
(format t "   AI Command: trace_function~%")
(format t "   {~%")
(format t "     \"tool\": \"trace_function\",~%")
(format t "     \"arguments\": {\"functionName\": \"factorial\"}~%")
(format t "   }~%")
(format t "~%")

;; Step 4: Demonstrate tracing (using Lisp's trace for demo)
(format t "[STEP 4] Demonstrating TRACE (from CL Cookbook)~%")
(format t "~%")
(format t "   The CL Cookbook shows how trace reveals recursion:~%")
(format t "~%")
(format t "   (trace factorial)~%")
(format t "   (factorial 3)~%")
(format t "~%")
(format t "   Expected output:~%")
(format t "     0: (FACTORIAL 3)~%")
(format t "       1: (FACTORIAL 2)~%")
(format t "         2: (FACTORIAL 1)~%")
(format t "           3: (FACTORIAL 0)~%")
(format t "           3: FACTORIAL returned 1~%")
(format t "         2: FACTORIAL returned 1~%")
(format t "       1: FACTORIAL returned 2~%")
(format t "     0: FACTORIAL returned 6~%")
(format t "~%")

;; Actually trace it
(trace factorial)
(format t "   Actual trace output:~%")
(let ((result (factorial 3)))
  (format t "   (factorial 3) => ~d~%" result))
(untrace factorial)
(format t "~%")

;; Step 5: Show AI agent equivalent using CL-TRON-MCP tools
(format t "[STEP 5] CL-TRON-MCP AI Tools for This Example~%")
(format t "~%")
(format t "   Instead of manually calling trace, the AI agent uses:~%")
(format t "~%")
(format t "   1. trace_function - Start tracing~%")
(format t "   2. repl_eval     - Call (factorial 5)~%")
(format t "   3. trace_list    - Check what's traced~%")
(format t "   4. trace_remove  - Stop tracing~%")
(format t "~%")
(format t "   JSON-RPC messages from AI agent:~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"trace_function\",~%")
(format t "     \"arguments\": {\"functionName\": \"factorial\"}~%")
(format t "   }~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"repl_eval\",~%")
(format t "     \"arguments\": {\"code\": \"(factorial 5)\" \"package\": \"CL-USER\"}~%")
(format t "   }~%")
(format t "~%")

;; Step 6: Cross-reference example from CL Cookbook
(format t "[STEP 6] Cross-Reference Analysis (from CL Cookbook)~%")
(format t "~%")
(format t "   The CL Cookbook shows how to find who calls a function:~%")
(format t "   In SLIME: C-c C-w C-c (slime-who-calls)~%")
(format t "~%")
(format t "   CL-TRON-MCP equivalent:~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"who_calls\",~%")
(format t "     \"arguments\": {\"symbolName\": \"factorial\"}~%")
(format t "   }~%")
(format t "~%")

;; Step 7: Inspector example
(format t "[STEP 7] Inspecting the Factorial Function~%")
(format t "~%")
(format t "   AI agent can inspect the function definition:~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"inspect_function\",~%")
(format t "     \"arguments\": {\"functionName\": \"factorial\"}~%")
(format t "   }~%")
(format t "~%")
(format t "   Returns: function type, argument list, docstring, source location~%")
(format t "~%")

;; Demonstrate inspect
(format t "   Manual inspection output:~%")
(format t "   Name: FACTORIAL~%")
(format t "   Type: FUNCTION~%")
(format t "   Arguments: (N)~%")
(format t "   Documentation: \"Compute the factorial of N recursively.\"~%")
(format t "~%")

;; Step 8: Debugger integration from CL Cookbook
(format t "[STEP 8] Interactive Debugger (from CL Cookbook)~%")
(format t "~%")
(format t "   The CL Cookbook demonstrates entering the debugger:~%")
(format t "   - Use (break) to enter debugger~%")
(format t "   - View backtrace with restarts~%")
(format t "   - Restart frames to fix bugs without restart~%")
(format t "~%")
(format t "   CL-TRON-MCP debugger tools:~%")
(format t "~%")
(format t "   1. debugger_frames  - Get stack frames~%")
(format t "   2. debugger_restarts - List available restarts~%")
(format t "   3. breakpoint_set  - Set conditional breakpoint~%")
(format t "   4. step_frame     - Step through code~%")
(format t "~%")
(format t "   Example: Break when n=0~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"set_breakpoint\",~%")
(format t "     \"arguments\": {~%")
(format t "       \"functionName\": \"factorial\",~%")
(format t "       \"condition\": \"(equal 0 n)\"~%")
(format t "     }~%")
(format t "   }~%")
(format t "~%")

;; Step 9: Compile with debug info
(format t "[STEP 9] Compile with Debug Information~%")
(format t "~%")
(format t "   For best debugging, compile with maximum debug settings:~%")
(format t "~%")
(format t "   (declaim (optimize (speed 0) (space 0) (debug 3)))~%")
(format t "~%")
(format t "   AI agent command:~%")
(format t "~%")
(format t "   {~%")
(format t "     \"tool\": \"repl_eval\",~%")
(format t "     \"arguments\": {\"code\": \"(declaim (optimize (speed 0) (space 0) (debug 3)))\"~%")
(format t "   }~%")
(format t "~%")

;; Actually do it
(declaim (optimize (speed 0) (space 0) (debug 3)))
(format t "   Declared: optimize (speed 0) (space 0) (debug 3)~%")
(format t "~%")

;; Step 10: Performance profiling
(format t "[STEP 10] Performance Profiling~%")
(format t "~%")
(format t "   AI agent can profile the factorial function:~%")
(format t "~%")
(format t "   1. { \"tool\": \"profile_start\" }~%")
(format t "   2. Call (factorial 10000) multiple times~%")
(format t "   3. { \"tool\": \"profile_stop\" }~%")
(format t "   4. { \"tool\": \"profile_report\" }~%")
(format t "~%")
(format t "   This helps identify performance bottlenecks.~%")
(format t "~%")

;; Step 11: Summary
(format t "========================================~%")
(format t "  Tutorial Complete!~%")
(format t "========================================~%")
(format t "~%")
(format t "Summary of AI Agent Capabilities Demonstrated:~%")
(format t "~%")
(format t "  - trace_function: Trace function calls~%")
(format t "  - repl_eval: Evaluate Lisp code~%")
(format t "  - who_calls: Find callers (xref)~%")
(format t "  - inspect_function: Inspect definitions~%")
(format t "  - debugger_frames: Get stack frames~%")
(format t "  - debugger_restarts: List restarts~%")
(format t "  - breakpoint_set: Set breakpoints~%")
(format t "  - profile_*: Performance profiling~%")
(format t "~%")
(format t "Total CL-TRON-MCP Tools: 43 across 11 categories~%")
(format t "~%")
(format t "========================================~%")
(format t "~%")

;; Clean up
(format t "[CLEANUP] Removing test function...~%")
(unintern 'factorial :cl-user)
(format t "~%Tutorial finished successfully!~%")
