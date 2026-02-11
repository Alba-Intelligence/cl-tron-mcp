# CL-TRON-MCP Tutorial

This tutorial provides step-by-step instructions for using CL-TRON-MCP to debug Common Lisp applications.

## Tutorial Files

- `debugging-tutorial.lisp` - Lisp code examples to follow along
- `tutorial.json` - JSON format tutorial for MCP clients

## Quick Start

### 1. Load CL-TRON-MCP

```lisp
(ql:quickload :cl-tron-mcp)
```

### 2. Start the MCP Server

```lisp
;; Stdio transport (recommended for CLI tools)
(cl-tron-mcp:start-server :transport :stdio)

;; HTTP transport (for web-based tools)
(cl-tron-mcp:start-server :transport :http :port 8080)
```

### 3. Run Through the Tutorial

Load the tutorial code and follow along:

```lisp
;; Load the tutorial
(load "tutorial/debugging-tutorial.lisp")

;; Or use the JSON tutorial with your MCP client
;; Each scenario contains multiple tool calls
```

## Tutorial Scenarios

### 1. Setup and Inspect
- Inspect function definitions
- Explore package contents
- Examine CLOS classes

### 2. Trace Function Calls
- Start tracing a function
- View call stack
- Remove traces

### 3. Cross-Reference Analysis
- Find who calls a function
- List called functions
- Find variable references

### 4. Interactive Debugging
- Evaluate code in REPL context
- Inspect stack frames
- List available restarts

### 5. Logging Setup
- Configure log levels per package
- Add debug/info/warn/error logs
- Control logging output

### 6. Hot Code Reload
- Compile and load code strings
- Reload ASDF systems
- Fix bugs without restart

### 7. Performance Profiling
- Start/stop profiling
- Generate reports
- Analyze performance

### 8. Thread Management
- List all threads
- Inspect thread state
- Get thread backtraces

### 9. System Monitoring
- Health checks
- Runtime statistics
- Memory and GC info

### 10. Approval Whitelist
- Add patterns to whitelist
- Enable/disable approval
- Automate with confidence

## Example: Debugging Factorial

```lisp
;; Define a buggy function
(defun factorial (n)
  (if (plusp n)
      (* n (factorial (1- n)))
      1))

;; Inspect it
#{
  "tool": "inspect_function",
  "arguments": {"symbolName": "factorial"}
}#

;; Trace it
#{
  "tool": "trace_function",
  "arguments": {"functionName": "factorial"}
}#

;; Call it and see trace output
(factorial 5)  ;; => 120

;; Cross-reference analysis
#{
  "tool": "who_calls",
  "arguments": {"symbolName": "factorial"}
}#

;; Fix and reload
#{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(defun factorial (n)
  (cond
    ((plusp n) (* n (factorial (1- n))))
    ((zerop n) 1)
    (t (error \"Negative!\"))))",
    "filename": "factorial.lisp"
  }
}#
```

## Using with AI Agents

CL-TRON-MCP is designed for both human developers and AI agents:

### For Human Developers
- Interactive REPL debugging
- Visual inspection tools
- Easy-to-use MCP interface

### For AI Agents
- Comprehensive tool suite
- Approval whitelist for automation
- Stateless tool calls

### Automation Example

```json
{
  "tool": "whitelist_add",
  "arguments": {
    "operation": "eval",
    "pattern": "test-*"
  }
}
```

Then run automated tests without approval prompts.

## Next Steps

1. Review the [API Documentation](../docs/tools/)
2. Check the [Cookbook Examples](https://lispcookbook.github.io/cl-cookbook/debugging.html)
3. Explore the [AGENTS.md](../AGENTS.md) for AI agent guidelines

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Can't connect to server | Check port 8080 or use stdio transport |
| Approval timeout | Add pattern to whitelist |
| Missing features | Rebuild SBCL with :sb-dbg for debugger features |
| Tests failing | `(asdf:compile-system :cl-tron-mcp :force t)` |
