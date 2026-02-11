;;;; CL-TRON-MCP Tutorial: Debugging a Real-World Scenario

;; This tutorial demonstrates using CL-TRON-MCP to debug a buggy factorial function.
;; Follow along step by step to learn the debugging workflow.

;; ============================================================================
;; STEP 1: Setup - Load CL-TRON-MCP
;; ============================================================================

;; First, load CL-TRON-MCP in your Lisp image:
(ql:quickload :cl-tron-mcp)

;; Start the MCP server (in another terminal or as a background process):
;; (cl-tron-mcp:start-server :transport :stdio)

;; ============================================================================
;; STEP 2: Define a Buggy Function
;; ============================================================================

;; Let's create a function with a bug, similar to the CL Cookbook example:

(defpackage :tutorial
  (:use :cl))

(in-package :tutorial)

(defun factorial (n)
  "Calculate factorial of N. This has a subtle bug!"
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

;; Test it:
(factorial 5)  ;; => 120 (correct)
(factorial 0)  ;; => 1 (correct)

;; But what if we call it with a negative number?
;; (factorial -1)  ;; This will cause issues!

;; ============================================================================
;; STEP 3: Using the Inspector to Examine Objects
;; ============================================================================

;; CL-TRON-MCP Tool: inspect_object
;; Inspect the factorial function definition:
#{
  "tool": "inspect_function",
  "arguments": {
    "symbolName": "tutorial::factorial"
  }
}#

;; This returns information about the function type, lambda list, etc.

;; ============================================================================
;; STEP 4: Using the Tracer to Track Function Calls
;; ============================================================================

;; CL-TRON-MCP Tool: trace_function
;; Trace the factorial function to see the call stack:

#{
  "tool": "trace_function",
  "arguments": {
    "functionName": "tutorial::factorial"
  }
}#

;; Now call the function and observe the trace output:
;; (tutorial::factorial 3)

;; CL-TRON-MCP Tool: trace_list
;; Check which functions are being traced:

#{
  "tool": "trace_list",
  "arguments": {}
}#

;; CL-TRON-MCP Tool: trace_remove
;; Remove the trace when done:

#{
  "tool": "trace_remove",
  "arguments": {
    "functionName": "tutorial::factorial"
  }
}#

;; ============================================================================
;; STEP 5: Using Cross-References to Find Callers
;; ============================================================================

;; CL-TRON-MCP Tool: who-calls
;; Find all functions that call factorial:

#{
  "tool": "who_calls",
  "arguments": {
    "symbolName": "tutorial::factorial"
  }
}#

;; CL-TRON-MCP Tool: list_callees
;; Find all functions that factorial calls:

#{
  "tool": "list_callees",
  "arguments": {
    "symbolName": "tutorial::factorial"
  }
}#

;; ============================================================================
;; STEP 6: Using the Debugger to Inspect Stack Frames
;; ============================================================================

;; CL-TRON-MCP Tool: debugger_frames
;; Get the current stack frames:

#{
  "tool": "debugger_frames",
  "arguments": {
    "start": 0,
    "end": 10
  }
}#

;; CL-TRON-MCP Tool: debugger_restarts
;; List available restarts:

#{
  "tool": "debugger_restarts",
  "arguments": {}
}#

;; ============================================================================
;; STEP 7: Using the REPL for Interactive Debugging
;; ============================================================================

;; CL-TRON-MCP Tool: repl_eval
;; Evaluate Lisp code in the context of your package:

#{
  "tool": "repl_eval",
  "arguments": {
    "code": "(in-package :tutorial)",
    "package": "CL-USER"
  }
}#

#{
  "tool": "repl_eval",
  "arguments": {
    "code": "(format t \"Testing factorial: ~d~%\" (factorial 5))",
    "package": "TUTORIAL"
  }
}#

;; ============================================================================
;; STEP 8: Using Logging
;; ============================================================================

;; CL-TRON-MCP Tool: log_configure
;; Configure logging for the tutorial package:

#{
  "tool": "log_configure",
  "arguments": {
    "level": "debug",
    "package": "tutorial"
  }
}#

;; CL-TRON-MCP Tool: log_debug
;; Add debug logging:

#{
  "tool": "log_debug",
  "arguments": {
    "message": "Calculating factorial",
    "package": "tutorial"
  }
}#

;; ============================================================================
;; STEP 9: Hot Reloading Code
;; ============================================================================

;; CL-TRON-MCP Tool: code_compile_string
;; Fix the bug and reload the function:

#{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(in-package :tutorial)
(defun factorial (n)
  \"Calculate factorial of N. Now handles negative numbers correctly!\"
  (cond
    ((plusp n) (* n (factorial (1- n))))
    ((zerop n) 1)
    (t (error \"Factorial undefined for negative numbers: ~d\" n))))",
    "filename": "factorial.lisp"
  }
}#

;; ============================================================================
;; STEP 10: Monitoring and System Information
;; ============================================================================

;; CL-TRON-MCP Tool: system_info
;; Get comprehensive system information:

#{
  "tool": "system_info",
  "arguments": {}
}#

;; CL-TRON-MCP Tool: runtime_stats
;; Get runtime statistics:

#{
  "tool": "runtime_stats",
  "arguments": {}
}#

;; CL-TRON-MCP Tool: health_check
;; Check the MCP server health:

#{
  "tool": "health_check",
  "arguments": {}
}#

;; ============================================================================
;; STEP 11: Thread Management
;; ============================================================================

;; CL-TRON-MCP Tool: thread_list
;; List all threads:

#{
  "tool": "thread_list",
  "arguments": {}
}#

;; CL-TRON-MCP Tool: thread_inspect
;; Inspect a specific thread:

#{
  "tool": "thread_inspect",
  "arguments": {
    "threadId": "main thread"
  }
}#

;; ============================================================================
;; STEP 12: Using the Approval System
;; ============================================================================

;; Operations that modify code require approval by default.
;; You can whitelist operations to bypass approval for automation.

;; CL-TRON-MCP Tool: whitelist_add
;; Add a pattern to whitelist:

#{
  "tool": "whitelist_add",
  "arguments": {
    "operation": "eval",
    "pattern": "tutorial::factorial*"
  }
}#

;; CL-TRON-MCP Tool: whitelist_status
;; Check whitelist status:

#{
  "tool": "whitelist_status",
  "arguments": {}
}#

;; ============================================================================
;; STEP 13: Using Profiler
;; ============================================================================

;; CL-TRON-MCP Tool: profile_start
;; Start profiling:

#{
  "tool": "profile_start",
  "arguments": {}
}#

;; Run some computations:
;; (tutorial::factorial 100)

;; CL-TRON-MCP Tool: profile_stop
;; Stop profiling:

#{
  "tool": "profile_stop",
  "arguments": {}
}#

;; CL-TRON-MCP Tool: profile_report
;; Get profiling report:

#{
  "tool": "profile_report",
  "arguments": {
    "format": "flat"
  }
}#

;; ============================================================================
;; Summary
;; ============================================================================

;; CL-TRON-MCP provides a comprehensive set of tools for:
;; - Object and function inspection
;; - Function tracing
;; - Cross-reference analysis
;; - Interactive debugging via REPL
;; - Hot code reloading
;; - Thread management
;; - Performance profiling
;; - Logging integration

;; These tools enable both human developers and AI agents to effectively
;; debug and analyze Common Lisp applications.
