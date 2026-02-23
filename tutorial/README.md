# CL-TRON-MCP Tutorials

This directory contains tutorials for CL-TRON-MCP, an MCP server for SBCL Common Lisp debugging, introspection, profiling, and hot code reloading.

## Tutorials

### 1. Quick Start: `tutorial.lisp`

Run the basic demo:
```lisp
(load "tutorial/tutorial.lisp")
```

Shows system info, runtime stats, and available tools.

### 2. Interactive Debugging: `factorial-demo.lisp`

Replicates the [Common Lisp Cookbook Debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html) examples with the factorial function.

```lisp
(load "tutorial/factorial-demo.lisp")
```

Demonstrates:
- Defining and testing the factorial function
- Using `trace` to reveal recursion
- CL-TRON-MCP AI agent commands for tracing
- Cross-reference analysis
- Function inspection
- Interactive debugger integration
- Performance profiling

### 3. Full IDE Setup: `CURSOR-MCP-TUTORIAL.md`

Comprehensive guide for setting up CL-TRON-MCP with Cursor IDE.

Contents:
- Installation instructions
- Cursor IDE MCP configuration
- Your first AI-assisted debug session
- Deep dive into tracing the factorial function
- Interactive debugger integration
- Advanced examples
- Complete tool reference

Read with any markdown viewer, or open in Cursor/VS Code.

## Quick Start with AI Agent

### Start the MCP Server

```bash
sbcl --non-interactive --noinform \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
```

### Example AI Agent Session

```
User: "Trace the factorial function and call (factorial 5)"

AI Agent:
1. trace_function {"functionName": "factorial"}
2. repl_eval {"code": "(factorial 5)"}
3. trace_list {}
4. trace_remove {"functionName": "factorial"}
```

## Tool Categories

| Category | Count | Description |
|----------|-------|-------------|
| Inspector | 5 | Object, class, function introspection |
| Debugger | 6 | Frames, restarts, breakpoints |
| REPL | 1 | Code evaluation with approval |
| Hot Reload | 2 | Live code modification |
| Profiler | 3 | Performance analysis |
| Tracer | 3 | Function tracing |
| Thread | 3 | Thread management |
| Monitor | 4 | Health, stats, GC, system info |
| Logging | 5 | log4cl integration |
| XRef | 5 | Cross-reference analysis |
| Security | 5 | Approval workflow |
| Swank | 13 | Swank server integration |
| Unified | 12 | Swank unified REPL interface |

**Total: 86+ tools** (table above is by category)

## Files

- `tutorial.lisp` - Basic demo
- `factorial-demo.lisp` - Interactive debugging tutorial
- `CURSOR-MCP-TUTORIAL.md` - Full IDE setup guide
- `debugging-tutorial.lisp` - Original debugging examples
- `tutorial.json` - MCP tool definitions for testing
- `swank-tutorial.lisp` - Swank integration tutorial
