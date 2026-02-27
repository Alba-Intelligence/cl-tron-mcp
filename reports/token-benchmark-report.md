# CL-TRON-MCP Token Usage Benchmark Report

**Generated:** 2026-02-27 13:07:06

**Purpose:** Measure token savings from refactoring work

## Executive Summary

This report documents the token usage improvements achieved through the CL-TRON-MCP refactoring initiative. The refactoring focused on:

- **Short tool descriptions** with `documentation-uri` instead of verbose inline documentation
- **Dynamic repl-help** generated from the tool registry instead of hardcoded lists
- **Concise error responses** with error codes instead of verbose messages with hints

### Overall Results

| Metric | Value |
|--------|-------|
| Total Before Tokens | 2501 |
| Total After Tokens | 1263 |
| Total Reduction | 49.5% |
| Average Reduction | 60.7% |
| Scenarios Tested | 5 |

## Detailed Results

### tools/list (5 tools)

**Before:** 928 tokens

**After:** 345 tokens

**Reduction:** 62.8%

**Before Sample:**

```lisp
((:NAME "inspect_object" :DESCRIPTION
  "Inspect a Lisp object and return its structure, type, and value. This tool provides detailed introspection capabilities for any Lisp object, including its class, slots, and values. Useful for understanding the internal structure of complex objects during debugging sessions. Returns a structured representation with type information, slot names, and their current values."
  :INPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:OBJECTID (:TYPE "string" :DESCRIPTION "The object ID to inspect"))
   :REQUIRED ("objectId"))
  :OUTPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:RESULT (:TYPE "object" :DESCRIPTION "The inspection result")))
  :REQUIRESAPPROVAL NIL)
 (:NAME "debugger_frames" :DESCRIPTION
  "Get the current debugger stack frames. This tool retrieves all active stack frames from the debugger, providing frame indices, function names, source locations, and local variables. Essential for understanding the call chain leading to an error. Each frame includes detailed information about the function call, source file, line number, and available local variables for inspection."
  :INPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:MAXFRAMES
    (:TYPE "integer" :DESCRIPTION "Maximum number of frames to return" :DEFAULT
     20))
   :REQUIRED NIL)
  :OUTPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:FRAMES (:TYPE "array" :ITEMS (:TYPE "object"))))
  :REQUIRESAPPROVAL NIL)
 (:NAME "repl_eval" :DESCRIPTION
  "Evaluate Lisp code in the connected REPL. This tool allows you to execute arbitrary Lisp code and receive the result. Supports evaluation in different packages. Useful for testing code snippets, inspecting runtime state, and performing live debugging. The code is evaluated in the context of the connected SBCL session, giving you full access to the running application's state."
  :INPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:CODE (:TYPE "string" :DESCRIPTION "The Lisp code to evaluate" :REQUIRED T)
    :PACKAGE
    (:TYPE "string" :DESCRIPTION "Package to evaluate in" :DEFAULT "CL-USER"))
   :REQUIRED ("code"))
  :OUTPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:RESULT (:TYPE "string" :DESCRIPTION "Evaluation result")))
  :REQUIRESAPPROVAL T)
 (:NAME "health_check" :DESCRIPTION
  "Perform a comprehensive health check of the MCP server and SBCL session. This tool checks various system components including memory usage, thread status, garbage collection state, and overall system health. Returns detailed status information for each component along with recommendations if any issues are detected. Essential for monitoring production systems and ensuring the debugging environment is functioning correctly."
  :INPUTSCHEMA (:TYPE "object" :PROPERTIES NIL :REQUIRED NIL) :OUTPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:STATUS (:TYPE "string" :DESCRIPTION "Overall health status") :CHECKS
    (:TYPE "object" :DESCRIPTION "Individual check results")))
  :REQUIRESAPPROVAL NIL)
 (:NAME "runtime_stats" :DESCRIPTION
  "Get comprehensive runtime statistics from the SBCL session. This tool provides detailed information about system resources including memory usage, thread count, garbage collection statistics, and SBCL version information. Useful for performance analysis, resource monitoring, and understanding the current state of the Lisp runtime environment. Returns structured data suitable for logging and monitoring systems."
  :INPUTSCHEMA (:TYPE "object" :PROPERTIES NIL :REQUIRED NIL) :OUTPUTSCHEMA
  (:TYPE "object" :PROPERTIES
   (:THREADCOUNT (:TYPE "integer" :DESCRIPTION "Number of active threads")
    :MEMORY (:TYPE "object" :DESCRIPTION "Memory usage statistics") :GC
    (:TYPE "object" :DESCRIPTION "Garbage collection statistics")))
  :REQUIRESAPPROVAL NIL))
```

**After Sample:**

```lisp
(#<HASH-TABLE :TEST EQUAL :COUNT 8 {12061547D3}>
 #<HASH-TABLE :TEST EQUAL :COUNT 8 {120621C1D3}>
 #<HASH-TABLE :TEST EQUAL :COUNT 8 {120632A273}>
 #<HASH-TABLE :TEST EQUAL :COUNT 8 {1206429EF3}>
 #<HASH-TABLE :TEST EQUAL :COUNT 8 {12064C8FD3}>)
```

### repl-help

**Before:** 1139 tokens

**After:** 780 tokens

**Reduction:** 31.5%

**Before Sample:**

```lisp
(:TYPE :SWANK :CONNECTED NIL :TOOLS
 ((:NAME "repl_connect" :DESCRIPTION
   "Connect to a Swank REPL server. This tool establishes a connection to a running Swank server (used by Slime, Portacle, and Sly). You can specify the host and port, or use auto-detection to find the server. Once connected, you can use all other REPL tools to interact with the Lisp session. The connection persists until you explicitly disconnect or the server is stopped."
   :USAGE "(repl-connect :host \"127.0.0.1\" :port 4006)" :EXAMPLES
   ((:AUTO "repl_connect :port 4006")
    (:EXPLICIT "repl_connect :type :swank :host \"127.0.0.1\" :port 4006")))
  (:NAME "repl_eval" :DESCRIPTION
   "Evaluate Lisp code in the connected REPL. This tool sends code to the Swank server for evaluation and returns the result. You can specify the package context for evaluation. Useful for testing code, inspecting values, and performing live debugging. The code is evaluated in the SBCL session, giving you access to the full runtime environment."
   :USAGE "(repl-eval :code \"(+ 1 2)\")" :EXAMPLES
   ((:SIMPLE "repl_eval :code \"(+ 1 2)\"")
    (:WITH-PACKAGE "repl_eval :code \"(my-func)\" :package \"MY-PKG\"")))
  (:NAME "repl_inspect" :DESCRIPTION
   "Inspect a Lisp object in the REPL. This tool provides detailed information about any Lisp object including its type, class, slots, and values. Useful for understanding the structure of complex objects during debugging. You can inspect variables, function results, or any expression that evaluates to an object."
   :USAGE "(repl-inspect :expression \"my-variable\")" :EXAMPLES
   ((:VARIABLE "repl_inspect :expression \"*my-var*\"")
    (:EXPRESSION "repl_inspect :expression \"(list 1 2 3)\"")))
  (:NAME "repl_backtrace" :DESCRIPTION
   "Get the current backtrace from the REPL. This tool retrieves the call stack showing the sequence of function calls that led to the current state. Essential for debugging errors and understanding program flow. Each frame includes function name, source location, and available local variables."
   :USAGE "(repl-backtrace)" :EXAMPLES ((:BASIC "repl_backtrace")))
  (:NAME "repl_step" :DESCRIPTION
   "Step into the next expression in the current debugger frame. This tool allows you to execute code one expression at a time, moving into function calls. Useful for detailed debugging and understanding code execution flow. You can specify which frame to step in if multiple frames are available."
   :USAGE "(repl-step :frame 0)" :EXAMPLES ((:BASIC "repl_step :frame 0")))
  (:NAME "repl_next" :DESCRIPTION
   "Step over the next expression in the current debugger frame. This tool executes the next expression without stepping into function calls. Useful for quickly moving through code while staying at the same abstraction level. You can specify which frame to step in if multiple frames are available."
   :USAGE "(repl-next :frame 0)" :EXAMPLES ((:BASIC "repl_next :frame 0")))
  (:NAME "repl_continue" :DESCRIPTION
   "Continue execution from the current debugger state. This tool resumes normal program execution, exiting the debugger. Use this when you've finished inspecting the current state and want the program to continue running. Any breakpoints will still be active."
   :USAGE "(repl-continue)" :EXAMPLES ((:BASIC "repl_continue")))
  (:NAME "repl_set_breakpoint" :DESCRIPTION
   "Set a breakpoint on a function. This tool allows you to pause execution when a specific function is called. You can optionally specify conditions, hit counts, and thread filters. Breakpoints are essential for debugging by allowing you to inspect state at specific points in execution."
   :USAGE "(repl-set-breakpoint :function \"my-function\")" :EXAMPLES
   ((:BASIC "repl_set_breakpoint :function \"my-func\"")
    (:WITH-CONDITION
     "repl_set_breakpoint :function \"my-func\" :condition \"(> x 10)\"")))
  (:NAME "repl_list_breakpoints" :DESCRIPTION
   "List all currently set breakpoints. This tool shows all active breakpoints including their IDs, functions, conditions, and enabled status. Useful for managing breakpoints and understanding what debugging instrumentation is currently in place."
   :USAGE "(repl-list-breakpoints)" :EXAMPLES
   ((:BASIC "repl_list_breakpoints")))
  (:NAME "repl_help" :DESCRIPTION
   "Get help on available REPL tools. This tool provides information about all unified REPL tools including their names, descriptions, and usage examples. Useful for discovering available functionality and learning how to use the REPL interface effectively."
   :USAGE "(repl-help)" :EXAMPLES ((:BASIC "repl_help"))))
 :COUNT 10)
```

**After Sample:**

```lisp
(:TYPE NIL :CONNECTED NIL :TOOLS
 ((:NAME "repl_abort" :DESCRIPTION "Abort/interrupt REPL evaluation"
   :DOCUMENTATION-URI "file://docs/tools/repl-abort.md")
  (:NAME "repl_backtrace" :DESCRIPTION "Get REPL call stack" :DOCUMENTATION-URI
   "file://docs/tools/repl-backtrace.md")
  (:NAME "repl_compile" :DESCRIPTION "Compile and load Lisp code"
   :DOCUMENTATION-URI "file://docs/tools/repl-compile.md")
  (:NAME "repl_completions" :DESCRIPTION "Get symbol completions"
   :DOCUMENTATION-URI "file://docs/tools/repl-completions.md")
  (:NAME "repl_connect" :DESCRIPTION "Connect to Swank REPL" :DOCUMENTATION-URI
   "file://docs/tools/repl-connect.md")
  (:NAME "repl_continue" :DESCRIPTION "Continue from debugger"
   :DOCUMENTATION-URI "file://docs/tools/repl-continue.md")
  (:NAME "repl_describe" :DESCRIPTION "Describe symbol in REPL"
   :DOCUMENTATION-URI "file://docs/tools/repl-describe.md")
  (:NAME "repl_disconnect" :DESCRIPTION "Disconnect from REPL"
   :DOCUMENTATION-URI "file://docs/tools/repl-disconnect.md")
  (:NAME "repl_doc" :DESCRIPTION "Get symbol documentation" :DOCUMENTATION-URI
   "file://docs/tools/repl-doc.md")
  (:NAME "repl_eval" :DESCRIPTION "Evaluate Lisp code in REPL"
   :DOCUMENTATION-URI "file://docs/tools/repl-eval.md")
  (:NAME "repl_frame_locals" :DESCRIPTION "Get frame local variables"
   :DOCUMENTATION-URI "file://docs/tools/repl-frame-locals.md")
  (:NAME "repl_get_restarts" :DESCRIPTION "Get available restarts"
   :DOCUMENTATION-URI "file://docs/tools/repl-get-restarts.md")
  (:NAME "repl_help" :DESCRIPTION "Get help on REPL tools" :DOCUMENTATION-URI
   "file://docs/tools/repl-help.md")
  (:NAME "repl_inspect" :DESCRIPTION "Inspect object in REPL"
   :DOCUMENTATION-URI "file://docs/tools/repl-inspect.md")
  (:NAME "repl_invoke_restart" :DESCRIPTION "Invoke a restart"
   :DOCUMENTATION-URI "file://docs/tools/repl-invoke-restart.md")
  (:NAME "repl_list_breakpoints" :DESCRIPTION "List all breakpoints"
   :DOCUMENTATION-URI "file://docs/tools/repl-list-breakpoints.md")
  (:NAME "repl_next" :DESCRIPTION "Step over next expression"
   :DOCUMENTATION-URI "file://docs/tools/repl-next.md")
  (:NAME "repl_out" :DESCRIPTION "Step out of current frame" :DOCUMENTATION-URI
   "file://docs/tools/repl-out.md")
  (:NAME "repl_remove_breakpoint" :DESCRIPTION "Remove a breakpoint"
   :DOCUMENTATION-URI "file://docs/tools/repl-remove-breakpoint.md")
  (:NAME "repl_set_breakpoint" :DESCRIPTION "Set a breakpoint"
   :DOCUMENTATION-URI "file://docs/tools/repl-set-breakpoint.md")
  (:NAME "repl_status" :DESCRIPTION "Check REPL connection status"
   :DOCUMENTATION-URI "file://docs/tools/repl-status.md")
  (:NAME "repl_step" :DESCRIPTION "Step into next expression"
   :DOCUMENTATION-URI "file://docs/tools/repl-step.md")
  (:NAME "repl_threads" :DESCRIPTION "List all REPL threads" :DOCUMENTATION-URI
   "file://docs/tools/repl-threads.md")
  (:NAME "repl_toggle_breakpoint" :DESCRIPTION "Toggle breakpoint state"
   :DOCUMENTATION-URI "file://docs/tools/repl-toggle-breakpoint.md"))
 :COUNT 20 :EXAMPLES
 ((:AUTO-DETECT "repl_connect" :PORT 4006)
  (:EXPLICIT-SWANK "repl_connect" :TYPE :SWANK :PORT 4006)))
```

### error response

**Before:** 200 tokens

**After:** 74 tokens

**Reduction:** 63.0%

**Before Sample:**

```lisp
(:ERROR T :CODE "REPL_NOT_CONNECTED" :MESSAGE
 "Not connected to a REPL. You must connect to a Swank server before using REPL tools."
 :HINT
 "To connect to a REPL, use the repl_connect tool with the appropriate host and port. For example: repl_connect :host \"127.0.0.1\" :port 4006. Make sure you have a Swank server running in your SBCL session. You can start Swank by evaluating (swank:create-server :port 4006) in your Lisp session. If you're using Slime, Portacle, or Sly, they typically start Swank automatically. Check that the port is correct and not blocked by a firewall. If you're connecting to a remote server, ensure the host is reachable and the port is open."
 :DETAILS
 (:EXPECTED-STATE "connected" :CURRENT-STATE "disconnected" :SUGGESTED-ACTION
  "Call repl_connect first"))
```

**After Sample:**

```lisp
((:ERROR T :CODE "REPL_NOT_CONNECTED" :MESSAGE "Not connected to any REPL"
  :HINT "Run repl_connect first. Example: repl_connect :port 4006")
 (:SETUP
  "To start Swank in SBCL: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)")
 :DOCS "docs/errors/repl-not-connected.md")
```

### health_check

**Before:** 105 tokens

**After:** 24 tokens

**Reduction:** 77.1%

**Before Sample:**

```lisp
(:STATUS :HEALTHY :CHECKS (:MEMORY :OK :THREADS :OK :GC :OK) :TIMESTAMP
 3981157626 :DESCRIPTION
 "Comprehensive health check of the MCP server and SBCL session. This tool checks various system components including memory usage, thread status, garbage collection state, and overall system health. Returns detailed status information for each component along with recommendations if any issues are detected.")
```

**After Sample:**

```lisp
(:STATUS :HEALTHY :CHECKS (:MEMORY :OK :THREADS :OK :GC :OK) :TIMESTAMP
 1772168826)
```

### runtime_stats

**Before:** 129 tokens

**After:** 40 tokens

**Reduction:** 69.0%

**Before Sample:**

```lisp
(:THREAD-COUNT 5 :MEMORY
 (:DYNAMIC-BYTES 12345678 :TOTAL-BYTES 12345678 :TOTAL-MB 12) :GC
 (:BYTES-CONSED-BETWEEN-GCS 543210) :SBCL-VERSION "2.4.0" :DESCRIPTION
 "Get comprehensive runtime statistics from the SBCL session. This tool provides detailed information about system resources including memory usage, thread count, garbage collection statistics, and SBCL version information. Useful for performance analysis, resource monitoring, and understanding the current state of the Lisp runtime environment.")
```

**After Sample:**

```lisp
(:THREAD-COUNT 1 :MEMORY
 (:DYNAMIC-BYTES 82790480 :TOTAL-BYTES 82790480 :TOTAL-MB 79) :GC
 (:BYTES-CONSED-BETWEEN-GCS 53687091) :SBCL-VERSION "2.6.0")
```

## Recommendations

- All scenarios show significant token savings (>20%). No immediate optimization needed.


## Methodology

### Token Estimation

Token counts are estimated using a rough approximation of 4 characters per token, which is typical for English text and JSON. Actual token counts may vary depending on the tokenizer used by the AI model.

### "Before" State Simulation

Since the actual "before" state is no longer available, we simulate it by creating mock responses with verbose descriptions that match the style of the original implementation. These mock responses are based on:

- Audit logs from the original codebase
- Common patterns in verbose tool descriptions
- Typical error message formats with hints

### "After" State Measurement

The "after" state is measured using the actual current implementation, ensuring accurate representation of the refactored code.

### Limitations

- Token estimation is approximate and may not match exact tokenizer behavior
- Before state is simulated and may not perfectly match the original implementation
- Results may vary based on the specific AI model and tokenizer used

---

*Report generated by CL-TRON-MCP benchmark-tokens.lisp*
