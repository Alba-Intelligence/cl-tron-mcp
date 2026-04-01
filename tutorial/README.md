# CL-TRON-MCP Tutorials

This directory contains tutorials for CL-TRON-MCP, an MCP server for SBCL Common Lisp debugging, introspection, profiling, and hot code reloading.

## Start Here: No Setup Required

As of v0.1.0, CL-TRON-MCP can bootstrap its own SBCL+Swank subprocess via the `swank_launch` tool. You no longer need to start a Swank server manually.

```
AI Agent: swank_launch {"port": 4006}
→ {:success true, :port 4006, :pid ..., :message "SBCL+Swank running on port 4006"}

AI Agent: repl_connect {"port": 4006}
→ {:connected true, :port 4006}

AI Agent: repl_eval {"code": "(+ 1 2)"}
→ {:result "3"}
```

## Tutorials

### 1. Canonical Debugging Workflow: `f1-f2-tutorial.lisp`

The definitive tutorial. Demonstrates the full agent-interactive debugging loop:

```lisp
;; Step 0: Bootstrap Swank (tool: swank_launch)
;; Step 1: Connect (tool: repl_connect)
;; Step 2: Compile f1 which calls undefined f2 → warning
;; Step 3: Call (f1 1 2) → UNDEFINED-FUNCTION debugger
;; Step 4: Inspect frames and restarts
;; Step 5: Hot-compile f2 into the running image
;; Step 6: Resume via RETRY restart → result 3
;; Step 7: Verify (f1 1 2) = 3
```

This tutorial has an automated integration test:
```bash
sbcl --noinform --disable-debugger \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(ql:quickload :rove :silent t)' \
  --eval '(ql:quickload :cl-tron-mcp/tests/integration :silent t)' \
  --eval '(rove:run :cl-tron-mcp/tests/integration)'
```

### 2. Quick Start: `tutorial.lisp`

Load and run a basic smoke test of all tool categories:

```lisp
(load "tutorial/tutorial.lisp")
```

### 3. Interactive Debugging: `factorial-demo.lisp`

Replicates the [Common Lisp Cookbook Debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html) examples with the factorial function.

```lisp
(load "tutorial/factorial-demo.lisp")
```

Demonstrates: tracing, cross-reference, inspection, debugger, profiling.

### 4. End-to-End MCP Workflow: `e2e-mcp-workflow.md`

Copy-paste reference for agents. Zero to first eval in one page.

### 5. Unified REPL Tutorial: `unified-tutorial.lisp`

Documents the `repl_*` unified interface tools (Swank-backed).

### 6. Interactive Debugging Reference: `debugging-tutorial.lisp`

Step-by-step runnable Lisp file showing inspector, tracer, debugger, and hot-reload.

### 7. Swank Integration: `swank-tutorial.lisp`

Deep dive into the Swank connection layer and low-level `swank_*` tools.

### 8. Full IDE Setup: `CURSOR-MCP-TUTORIAL.md`

Comprehensive guide for setting up CL-TRON-MCP with Cursor IDE.

## Quick Start (MCP Server)

```bash
# Start as stdio MCP server (for Cursor, VS Code, OpenCode, Kilocode):
sbcl --noinform --disable-debugger \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'

# Or use the start script:
./start-mcp.sh
```

## Example AI Agent Session

```
User: "Define a function f1 that calls f2, then debug what happens when f2 is missing"

AI Agent:
1. swank_launch    {"port": 4006}               ; bootstrap SBCL
2. repl_connect    {"port": 4006}               ; connect
3. repl_compile    {"code": "(defun f1 (a b) (f2 a b))"}   ; → warning: F2 undefined
4. repl_eval       {"code": "(f1 1 2)"}         ; → UNDEFINED-FUNCTION F2 debugger
5. repl_backtrace  {}                           ; inspect frames
6. repl_get_restarts {}                         ; find RETRY restart
7. repl_compile    {"code": "(defun f2 (x y) (+ x y))"}    ; hot-compile f2
8. repl_invoke_restart {"restart_index": 0}     ; resume → returns 3
9. repl_eval       {"code": "(f1 1 2)"}         ; verify → "3"
10. swank_kill     {"port": 4006}               ; optional cleanup
```

## Tool Categories

| Category | Count | Description |
|----------|-------|-------------|
| Inspector | 5 | Object, slot, class, function, package |
| Debugger | 6 | Frames, restarts, breakpoints, stepping |
| REPL | 1 | Code evaluation (approval required) |
| Hot Reload | 2 | Live code modification without restart |
| Profiler | 3 | Statistical performance analysis |
| Tracer | 3 | Function call tracing |
| Thread | 3 | Thread list, inspect, backtrace |
| Monitor | 4 | Health check, runtime stats, GC, system info |
| Logging | 5 | log4cl integration |
| XRef | 5 | who-calls, who-references, list-callees, etc. |
| Security | 5 | Approval workflow, whitelist management |
| Swank (raw) | 21 | Low-level Swank RPC tools |
| Unified REPL | 23 | High-level repl_* tools (recommended) |
| Process Mgmt | 4 | swank_launch, swank_kill, process list/status |

**Total: 91 tools across 14 categories**

## Files

| File | Type | Description |
|------|------|-------------|
| `f1-f2-tutorial.lisp` | docs | Canonical debugging workflow (auto-testable) |
| `tutorial.lisp` | runnable | Quick smoke test of all categories |
| `factorial-demo.lisp` | runnable | CL Cookbook factorial debugging example |
| `e2e-mcp-workflow.md` | docs | Copy-paste workflow reference for agents |
| `unified-tutorial.lisp` | docs | Unified repl_* interface guide |
| `debugging-tutorial.lisp` | runnable | Inspector/tracer/debugger/hot-reload demo |
| `swank-tutorial.lisp` | docs | Swank layer deep dive |
| `CURSOR-MCP-TUTORIAL.md` | docs | Full Cursor IDE setup guide |
| `tutorial.json` | data | MCP tool definitions for testing |
| `tutorial-run.lsp` | runnable | Automated tool smoke test |
