# CL-TRON-MCP: AI-Powered Common Lisp Debugging with Cursor IDE

This tutorial demonstrates how to set up and use CL-TRON-MCP with Cursor IDE for AI-assisted Common Lisp debugging. CL-TRON-MCP provides 43 tools that enable AI agents to interact with running Lisp sessions as if they were a human developer at the keyboard.

## Table of Contents

1. [Installation](#installation)
2. [Cursor IDE Setup](#cursor-ide-setup)
3. [Your First AI-Assisted Debug Session](#your-first-ai-assisted-debug-session)
4. [Deep Dive: Tracing the Factorial Function](#deep-dive-tracing-the-factorial-function)
5. [Interactive Debugger Integration](#interactive-debugger-integration)
6. [Advanced Examples](#advanced-examples)

---

## Installation

### Prerequisites

- **SBCL** (Steel Bank Common Lisp) 2.0 or later
- **Quicklisp** (for dependency management)
- **Cursor IDE** (or any MCP-compatible editor)

### Install CL-TRON-MCP

```lisp
;; Clone or navigate to your local-projects directory
;; Then load the system:
(ql:quickload :cl-tron-mcp)

;; Verify installation
(format t "Version: ~a~%" cl-tron-mcp/core:*version*)
(format t "Tools registered: ~d~%" (hash-table-count cl-tron-mcp/tools:*tool-registry*))
```

Expected output:
```
Version: 0.1.0
Tools registered: 43
```

---

## Cursor IDE Setup

### Option 1: Using Cursor's MCP Configuration

1. Open Cursor Settings (`Cmd+,` or `Ctrl+,`)
2. Navigate to **MCP (Model Context Protocol)**
3. Add a new MCP server:

```json
{
  "name": "cl-tron-mcp",
  "command": "sbcl",
  "args": [
    "--non-interactive",
    "--eval", "(ql:quickload :cl-tron-mcp :silent t)",
    "--eval", "(cl-tron-mcp/core:start-server :transport :stdio)"
  ],
  "enabled": true
}
```

### Option 2: Using a Shell Script (Recommended)

Create `start-mcp.sh` in the cl-tron directory:

```bash
#!/bin/bash
cd "$(dirname "$0")"
exec sbcl --non-interactive \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
```

Make it executable:
```bash
chmod +x start-mcp.sh
```

Then in Cursor MCP settings:
```json
{
  "name": "cl-tron-mcp",
  "command": "/path/to/start-mcp.sh",
  "enabled": true
}
```

### Verify Connection

After configuring Cursor, restart it. You should see CL-TRON-MCP tools available in the MCP panel.

---

## Your First AI-Assisted Debug Session

### Step 1: Create a Lisp Source File

Create `factorial.lisp`:

```lisp
;;; factorial.lisp - Example from the Common Lisp Cookbook

(defpackage :factorial-demo
  (:use :cl))

(in-package :factorial-demo)

(defun factorial (n)
  "Compute the factorial of N recursively."
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

;;; Test the function
(factorial 5)
;; Expected: 120
```

### Step 2: Load the File in Your Lisp Image

```lisp
;; Load the source file
(load "factorial.lisp")

;; Test it
(factorial 5)
;; => 120
```

### Step 3: Ask Your AI Assistant to Trace It

In Cursor, ask:

> "Trace the `factorial` function and call `(factorial 5)` to see the recursion."

The AI agent will use CL-TRON-MCP tools:

```json
{
  "tool": "trace_function",
  "arguments": {
    "functionName": "factorial-demo:factorial"
  }
}
```

Then execute:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(factorial-demo:factorial 5)"
  }
}
```

Expected trace output:
```
  0: (FACTORIAL-DEMO:FACTORIAL 5)
    1: (FACTORIAL-DEMO:FACTORIAL 4)
      2: (FACTORIAL-DEMO:FACTORIAL 3)
        3: (FACTORIAL-DEMO:FACTORIAL 2)
          4: (FACTORIAL-DEMO:FACTORIAL 1)
            5: (FACTORIAL-DEMO:FACTORIAL 0)
            5: FACTORIAL-DEMO:FACTORIAL returned 1
          4: FACTORIAL-DEMO:FACTORIAL returned 1
        3: FACTORIAL-DEMO:FACTORIAL returned 2
      2: FACTORIAL-DEMO:FACTORIAL returned 6
    1: FACTORIAL-DEMO:FACTORIAL returned 24
  0: FACTORIAL-DEMO:FACTORIAL returned 120
```

---

## Deep Dive: Tracing the Factorial Function

This section replicates the examples from the [Common Lisp Cookbook Debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html) page.

### Basic Tracing

```lisp
;; Define factorial (if not already loaded)
(defun factorial (n)
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

;; Start tracing
(trace factorial)

;; Call the function
(factorial 3)
;; Output:
;;   0: (FACTORIAL 3)
;;     1: (FACTORIAL 2)
;;       2: (FACTORIAL 1)
;;         3: (FACTORIAL 0)
;;         3: FACTORIAL returned 1
;;       2: FACTORIAL returned 1
;;     1: FACTORIAL returned 2
;;   0: FACTORIAL returned 6

;; Stop tracing
(untrace factorial)
```

### AI Agent Commands for Tracing

```json
{
  "tool": "trace_function",
  "arguments": {
    "functionName": "factorial"
  }
}
```

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(factorial 5)"
  }
}
```

```json
{
  "tool": "trace_list",
  "arguments": {}
}
```

```json
{
  "tool": "trace_remove",
  "arguments": {
    "functionName": "factorial"
  }
}
```

### Trace with Break Condition

The CL Cookbook shows breaking when `factorial` is called with 0:

```lisp
(trace factorial :break (equal 0 (sb-debug:arg 0)))
```

AI Agent equivalent - use the debugger tool:
```json
{
  "tool": "set_breakpoint",
  "arguments": {
    "functionName": "factorial",
    "condition": "(equal 0 n)"
  }
}
```

### Trace Options Reference

| Option | Description | AI Tool |
|--------|-------------|---------|
| `:break` | Invoke debugger before call | `set_breakpoint` |
| `:break-after` | Invoke debugger after call | (future) |
| `:condition` | Trace only if condition true | (future) |
| `:wherein` | Trace only when called from | (future) |
| `:print` | Print additional info | (future) |

---

## Interactive Debugger Integration

### Step Into the Debugger

When an error occurs or you use `break`, the interactive debugger presents:

1. Error message
2. Available restarts
3. Backtrace

### Example: Debugging a Division Error

```lisp
(defun unsafe-divide (a b)
  (/ a b))

(unsafe-divide 10 0)
;; ERROR: division by zero
```

### AI Agent Can Inspect the Debugger

```json
{
  "tool": "debugger_frames",
  "arguments": {}
}
```

Returns stack frames:
```
0: (UNSAFE-DIVIDE 10 0)
   Locals:
     A = 10
     B = 0
1: (...) 
```

```json
{
  "tool": "debugger_restarts",
  "arguments": {}
}
```

Returns restarts:
```
0: [CONTINUE] Return nil
1: [ABORT] Return to top level
```

### Resume Execution from a Frame

```json
{
  "tool": "step_frame",
  "arguments": {
    "frameId": 0,
    "action": "continue"
  }
}
```

### Compile with Debug Info

For maximum debugging information:

```lisp
(declaim (optimize (speed 0) (space 0) (debug 3)))
```

AI Agent:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(declaim (optimize (speed 0) (space 0) (debug 3)))"
  }
}
```

---

## Advanced Examples

### Example 1: Cross-Reference Analysis

Find who calls factorial:

```json
{
  "tool": "who_calls",
  "arguments": {
    "symbolName": "factorial"
  }
}
```

### Example 2: Inspect Function Definition

```json
{
  "tool": "inspect_function",
  "arguments": {
    "functionName": "factorial"
  }
}
```

### Example 3: Runtime Statistics

```json
{
  "tool": "runtime_stats",
  "arguments": {}
}
```

Returns:
```json
{
  "threadCount": 4,
  "memory": {
    "totalMb": 256,
    "usedMb": 128
  },
  "gc": {
    "count": 42
  }
}
```

### Example 4: Hot Reload Modified Code

After editing `factorial.lisp`:

```json
{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(load \"factorial.lisp\")"
  }
}
```

Or reload the entire ASDF system:
```json
{
  "tool": "reload_system",
  "arguments": {
    "systemName": "factorial-demo"
  }
}
```

### Example 5: Set a Breakpoint and Debug

```json
{
  "tool": "set_breakpoint",
  "arguments": {
    "functionName": "factorial",
    "condition": "(> n 3)"
  }
}
```

Then:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(factorial 5)"
  }
}
```

The debugger will break when `n > 3`.

---

## Complete AI Agent Workflow Example

Here's a typical debugging session with an AI agent:

```
User: "Debug why factorial(1000) is slow."

AI Agent:
1. {
     "tool": "trace_function",
     "arguments": {"functionName": "factorial"}
   }
2. {
     "tool": "repl_eval",
     "arguments": {"code": "(factorial 1000)"}
   }
3. Analyze trace output showing deep recursion
4. {
     "tool": "trace_remove",
     "arguments": {"functionName": "factorial"}
   }
5. {
     "tool": "profile_start",
     "arguments": {}
   }
6. {
     "tool": "repl_eval",
     "arguments": {"code": "(factorial 1000)"}
   }
7. {
     "tool": "profile_stop",
     "arguments": {}
   }
8. {
     "tool": "profile_report",
     "arguments": {}
   }

User: "The recursion is too deep. Suggest iterative version."

AI Agent: Recommends:
```lisp
(defun factorial (n)
  (loop for i from 1 to n
        for result = 1 then (* result i)
        finally (return result)))
```
```

---

## Tool Reference

### Inspector Tools (5)

| Tool | Description |
|------|-------------|
| `inspect_object` | Inspect an object by ID |
| `inspect_slot` | Get/set slot values on objects |
| `inspect_class` | Inspect CLOS class definitions |
| `inspect_function` | Inspect function definitions |
| `inspect_package` | Inspect packages and list contents |

### Debugger Tools (6)

| Tool | Description |
|------|-------------|
| `debugger_frames` | Get debugger stack frames |
| `debugger_restarts` | List available restarts |
| `breakpoint_set` | Set breakpoint on function |
| `breakpoint_remove` | Remove breakpoint by ID |
| `breakpoint_list` | List all active breakpoints |
| `step_frame` | Step execution in a frame |

### REPL Tool (1)

| Tool | Description |
|------|-------------|
| `repl_eval` | Evaluate Lisp code (requires approval) |

### Hot Reload Tools (2)

| Tool | Description |
|------|-------------|
| `code_compile_string` | Compile and load code string |
| `reload_system` | Reload ASDF system |

### Profiler Tools (3)

| Tool | Description |
|------|-------------|
| `profile_start` | Start deterministic profiling |
| `profile_stop` | Stop profiling |
| `profile_report` | Get profiling report |

### Tracer Tools (3)

| Tool | Description |
|------|-------------|
| `trace_function` | Add trace to a function |
| `trace_remove` | Remove trace from a function |
| `trace_list` | List all traced functions |

### Thread Tools (3)

| Tool | Description |
|------|-------------|
| `thread_list` | List all threads |
| `thread_inspect` | Get thread details |
| `thread_backtrace` | Get thread backtrace |

### Monitor Tools (4)

| Tool | Description |
|------|-------------|
| `health_check` | Basic health check |
| `runtime_stats` | Runtime statistics |
| `gc_run` | Force garbage collection |
| `system_info` | Comprehensive system info |

### Logging Tools (5)

| Tool | Description |
|------|-------------|
| `log_configure` | Configure logging |
| `log_info` | Log info message |
| `log_debug` | Log debug message |
| `log_warn` | Log warning |
| `log_error` | Log error |

### XRef Tools (7)

| Tool | Description |
|------|-------------|
| `who_calls` | Find callers of symbol |
| `who_references` | Find references |
| `who_binds` | Find bindings |
| `who_sets` | Find setq/makunbound |
| `who_specializes` | Find method specializers |
| `list_callees` | List called functions |

### Security Tools (5)

| Tool | Description |
|------|-------------|
| `whitelist_add` | Add approval whitelist pattern |
| `whitelist_remove` | Remove pattern |
| `whitelist_clear` | Clear all patterns |
| `whitelist_enable` | Enable/disable whitelist |
| `whitelist_status` | Get whitelist status |

---

## Troubleshooting

### "Symbol not found"

Ensure the package is loaded:
```lisp
(ql:quickload :your-package)
```

### "Approval timeout"

Increase timeout or proceed without approval for development:
```json
{
  "tool": "whitelist_add",
  "arguments": {
    "operation": "eval",
    "pattern": ".*"
  }
}
```

### Tests failing

Recompile:
```lisp
(asdf:compile-system :cl-tron-mcp :force t)
```

---

## Summary

CL-TRON-MCP transforms Cursor IDE into a powerful AI-assisted Common Lisp development environment. The AI agent can:

1. **Inspect** objects, classes, functions, and packages
2. **Debug** with breakpoints, stack frames, and restarts
3. **Trace** function calls to understand program flow
4. **Profile** performance bottlenecks
5. **Hot-reload** code without restarting
6. **Analyze** code with cross-references

This enables the same interactive development experience that makes Lisp famous, but accessible to AI agents for automated debugging and code improvement.

---

## References

- [CL-TRON-MCP GitHub](https://github.com/anomalyco/cl-tron-mcp)
- [Common Lisp Cookbook: Debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html)
- [SBCL Manual: Function Tracing](http://www.sbcl.org/manual/index.html#Function-Tracing)
- [SBCL User Manual](http://www.sbcl.org/manual/)
