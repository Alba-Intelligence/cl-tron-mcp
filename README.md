# CL-TRON-MCP

**AI-Powered Debugging for Common Lisp**

A Model Context Protocol (MCP) server that gives AI assistants deep access to running SBCL Common Lisp applications—debugger, inspector, profiler, and hot code reload.

```
┌─────────────────┐         ┌─────────────────┐
│  SBCL + Swank   │◄───────►│   Tron (MCP)    │
│  (Port 4005)    │         │   (stdio)       │
│                 │         │                 │
│  Your code      │         │   AI Agent      │
│  Debugger       │◄───────►│   debugs        │
│  Threads        │         │   inspects      │
│  State lives    │         │   fixes         │
└─────────────────┘         └─────────────────┘
```

## The Demo

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ $ ai "Debug factorial-example.lisp and fix it"                              │
└─────────────────────────────────────────────────────────────────────────────┘

  🔧 Connecting to Swank on port 4005... ✓
  
  🔧 Running (factorial 7)...
      ⚠️  ERROR: The value NIL is not of type NUMBER
      📍 Backtrace: (FACTORIAL 2) ← (FACTORIAL 3) ← (FACTORIAL 4)...
  
  🔧 Inspecting frame 0...
      N = 2
  
  🐛 Bug found: Line 4 has (1) called as function, returns NIL
     (if (> n 1) (* n (factorial (- n 1)) (1)))
                                          ^^^
                                          Should be: 1 (base case)
  
  🔧 Hot-reloading fixed function...
     (if (> n 1) (* n (factorial (- n 1))) 1)
                                          └─ moved here
  
  🔧 Verifying...
      (factorial 7)  → 5040      ✓
      (factorial 10) → 3628800   ✓
  
  ✅ Done! Session preserved. Update your source file to persist the fix.

┌─────────────────────────────────────────────────────────────────────────────┐
│  Total time: 19 seconds | Tools used: 8 | Restarts: 0 (hot-reload)         │
└─────────────────────────────────────────────────────────────────────────────┘
```

**No restart. No lost state. The Lisp session keeps running.**

## Quick Start

### 1. Start a Swank Server

```lisp
;; In SBCL
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
```

### 2. Configure Your AI Client

**Cursor** (`~/.cursor/mcp.json`):
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["/path/to/cl-tron-mcp/start-mcp.sh"]
    }
  }
}
```

**Claude Code** (`~/.config/claude-code/mcp.json`):
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["sbcl", "--non-interactive", "--noinform",
        "--eval", "(ql:quickload :cl-tron-mcp :silent t)",
        "--eval", "(cl-tron-mcp/core:start-server :transport :stdio)"]
    }
  }
}
```

### 3. Start Debugging

Ask your AI: *"Connect to Swank on port 4005 and help me debug my code"*

## Features

| Category | What It Does | Tools |
|----------|--------------|-------|
| **Debug** | Backtrace, restarts, stepping, breakpoints | `swank_backtrace`, `swank_invoke_restart`, `swank_step` |
| **Inspect** | Objects, classes, functions, packages | `inspect_object`, `inspect_class`, `inspect_function` |
| **Evaluate** | Run code in the live session | `swank_eval`, `repl_eval` |
| **Hot-Reload** | Fix bugs without restart | `swank_eval` with `defun`, `code_compile_string` |
| **Profile** | Find performance bottlenecks | `profile_start`, `profile_stop`, `profile_report` |
| **Trace** | See function call flow | `trace_function`, `trace_list` |
| **XRef** | Find callers and callees | `who_calls`, `list_callees` |

**99 tools total** across 14 categories.

## Why Tron?

### The Problem

Traditional debugging with AI:
- AI suggests fixes → you apply → restart → test → repeat
- Lost state on every restart
- No visibility into running system
- Manual copy-paste between AI and REPL

### The Solution

With Tron:
- AI connects directly to your running Lisp
- Inspects live state (variables, threads, stack)
- Hot-reloads fixes instantly
- Session persists—no lost state

### The Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│                        Your Machine                               │
│                                                                   │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────────────┐ │
│  │   SBCL      │     │    Tron     │     │    AI Client        │ │
│  │   +Swank    │◄───►│    MCP      │◄───►│  (Cursor/Claude)    │ │
│  │             │     │   Server    │     │                     │ │
│  │ Your code   │     │  (stdio)    │     │  You ask questions  │ │
│  │ Debugger    │     │  99 tools   │     │  AI debugs directly │ │
│  │ State───────┼─────┼─────────────┼─────┼─────────────────────┤ │
│  └─────────────┘     └─────────────┘     └─────────────────────┘ │
│        ▲                                        │                 │
│        │                                        │                 │
│        └────────────────────────────────────────┘                 │
│              Same session, no restart                             │
└──────────────────────────────────────────────────────────────────┘
```

## Installation

### Quicklisp

```lisp
(ql:quickload :cl-tron-mcp)
```

### From Source

```bash
git clone https://github.com/yourusername/cl-tron-mcp.git
cd cl-tron-mcp
# Link to quicklisp local-projects or load directly
sbcl --eval '(load "cl-tron-mcp.asd")' --eval '(ql:quickload :cl-tron-mcp)'
```

## Tool Highlights

### Debugging Workflow

```lisp
;; AI triggers error
(swank-eval :code "(my-buggy-function 7)")
;; => ERROR with backtrace

;; AI inspects the error
(swank-backtrace)
;; => Frames showing where it failed

;; AI fixes and hot-reloads
(swank-eval :code "(defun my-buggy-function (x) (corrected x))")

;; AI aborts the error and verifies
(swank-invoke-restart :restart_index 2)  ; ABORT
(swank-eval :code "(my-buggy-function 7)")
;; => Correct result
```

### Profiling Workflow

```lisp
(profile-start)
;; Run your code
(swank-eval :code "(process-large-dataset *data*)")
(profile-stop)
(profile-report :format "flat")
;; => See which functions are slow
```

### Cross-Reference

```lisp
;; Find who calls a function
(who-calls :symbol_name "my-package:process")
;; => List of callers

;; Find what a function calls
(list-callees :symbol_name "my-package:process")
;; => List of callees
```

## MCP Client Setup

### Cursor IDE

1. Install MCP extension
2. Copy `.cursor/mcp.json` to `~/.cursor/mcp.json`
3. Restart Cursor
4. Open a Lisp project and ask the AI to debug

### VS Code

1. Install MCP extension
2. Copy `.vscode/mcp.json` to `~/.vscode/mcp.json`
3. Use the AI assistant with Tron tools

### Claude Code CLI

```bash
# Add to ~/.config/claude-code/mcp.json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["/path/to/cl-tron-mcp/start-mcp.sh"]
    }
  }
}
```

## Requirements

- **SBCL** 2.0.0 or later
- **Quicklisp**
- **Swank** (for Slime integration) or **cl-nrepl** (for nrepl)

## Documentation

- **[AGENTS.md](AGENTS.md)** - Quick start for AI agents
- **[docs/architecture.md](docs/architecture.md)** - How it works
- **[prompts/workflow-examples.md](prompts/workflow-examples.md)** - Step-by-step examples
- **[tutorial/](tutorial/)** - Tutorials and examples

## Testing

```lisp
(asdf:test-system :cl-tron-mcp)
```

Or with Rove:
```lisp
(ql:quickload :rove)
(rove:run :cl-tron-mcp/tests)
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Package not found" | `(ql:quickload :cl-tron-mcp)` first |
| Client shows "failed" | Use `start-mcp.sh` which handles stdio correctly |
| "Not connected to REPL" | Run `swank_connect` or `repl_connect` first |
| Tests fail with stale FASL | `(asdf:compile-system :cl-tron-mcp :force t)` |

## Contributing

1. Fork the repository
2. Create a feature branch
3. Follow guidelines in `AGENTS.md`
4. Add tests
5. Submit a pull request

## License

MIT-style. See LICENSE file.

## Resources

- [SBCL Documentation](http://www.sbcl.org/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [Swank/Slime](https://slime.common-lisp.dev/)