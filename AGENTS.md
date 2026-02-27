# SBCL Debugging MCP Repository Guidelines

This document provides guidelines for AI agents working on the SBCL Debugging MCP project - a Model Context Protocol server that enables deep debugging, introspection, profiling, and hot code reloading for SBCL Common Lisp applications.

## Quick Start

**What is Tron?** An MCP server that connects to a running SBCL Lisp session and provides debugging, code evaluation, inspection, profiling, and hot-reload capabilities.

### Discovering How to Use Tron

Tron exposes documentation and guided workflows via MCP standard mechanisms:

| Method           | Purpose                                      |
| ---------------- | -------------------------------------------- |
| `resources/list` | List available documentation files           |
| `resources/read` | Read a documentation file by URI             |
| `prompts/list`   | List available guided workflows              |
| `prompts/get`    | Get step-by-step instructions for a workflow |

**Recommended:** Start with `prompts/get` for `getting-started` workflow to learn the connection pattern.

## Modular Documentation

This guide has been modularized for better token efficiency. Each section is now a separate document:

### Core Documentation

- **[Getting Started](docs/agents/getting-started.md)** - First-time setup, connection patterns, and basic usage
- **[Workflows](docs/agents/workflows.md)** - Common workflows: evaluate, debug, inspect, profile, hot-reload
- **[Tool Reference](docs/agents/tool-reference.md)** - Complete tool catalog organized by category
- **[Conventions](docs/agents/conventions.md)** - Coding style, naming conventions, and best practices
- **[Transport & Logging](docs/agents/transport-logging.md)** - Transport modes, stdio requirements, and logging
- **[Troubleshooting](docs/agents/troubleshooting.md)** - Common issues and solutions
- **[Token Optimization](docs/agents/token-optimization.md)** - Principles for reducing token usage

### Quick Reference

**Essential Pattern: One Long-Running Session**

```
┌─────────────────┐         ┌─────────────────┐
│  SBCL + Swank   │◄───────►│   Tron (MCP)    │
│  (Port 4006)    │         │   (stdio)       │
│                 │         │                 │
│  Your code      │         │   AI Agent      │
│  Debugger state │         │   evaluates     │
│  Threads        │         │   inspects      │
└─────────────────┘         └─────────────────┘
```

**Never restart the SBCL session.** Tron connects as a client; all state lives in SBCL.

**Core Development Loop:**

```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

**MCP Protocol Methods:**

| Method           | Purpose                                                  |
| ---------------- | -------------------------------------------------------- |
| `resources/list` | List documentation files (AGENTS.md, docs/\*)            |
| `resources/read` | Read a documentation file by URI                         |
| `prompts/list`   | List guided workflows                                    |
| `prompts/get`    | Get step-by-step workflow instructions                   |
| `tools/list`     | List available tools                                     |
| `tools/call`     | Invoke a tool                                            |

| Prompt                | Purpose                                                                      |
| --------------------- | ---------------------------------------------------------------------------- |
| `discover-mcp`        | How to fully use this MCP without user explanation (ordered discovery steps) |
| `getting-started`     | How to connect to Swank and verify setup                                     |
| `debugging-workflow`  | Step-by-step error debugging                                                 |
| `hot-reload-workflow` | Live code modification without restart                                       |
| `profiling-workflow`  | Performance analysis workflow                                                |
| `token-optimization`  | Principles for reducing token usage                                          |
| `sbcl-debugging-expert` | Specialized SBCL debugging workflows                                         |
| `hot-reload-specialist` | Hot code reloading specialist workflows                                      |
| `performance-engineer` | Performance profiling and optimization workflows                             |
## Tool Categories (86 tools total)

| Category   | Purpose                | Key Tools                                                      |
| ---------- | ---------------------- | -------------------------------------------------------------- |
| Inspector  | Object introspection   | `inspect_object`, `inspect_class`, `inspect_function`          |
| Debugger   | Debugging operations   | `debugger_frames`, `debugger_restarts`, `breakpoint_set`       |
| REPL       | Code evaluation        | `repl_eval`, `repl_frame_locals`                               |
| Hot Reload | Live code modification | `code_compile_string`, `reload_system`                         |
| Profiler   | Performance analysis   | `profile_start`, `profile_stop`                                |
| Tracer     | Function tracing       | `trace_function`, `trace_list`                                 |
| Threads    | Thread management      | `thread_list`, `thread_inspect`, `thread_backtrace`            |
| Monitor    | Production monitoring  | `health_check`, `runtime_stats`                                |
| Logging    | Package logging        | `log_configure`, `log_info`                                    |
| XRef       | Cross-reference        | `who_calls`, `who_references`, `list_callees`                  |
| Security   | Approval whitelist     | `whitelist_add`, `whitelist_status`                            |
| Swank      | Slime integration (21) | `swank_connect`, `swank_eval`, `swank_backtrace`, `swank_step` |
| Unified    | Swank REPL             | `repl_connect`, `repl_eval`, `repl_step`, `repl_continue`      |

## Project Structure

```
cl-tron-mcp/
├── src/
│   ├── core/                    # Core infrastructure (config, utils, version, server)
│   ├── transport/              # Transport layer (stdio, http via Hunchentoot, websocket)
│   ├── protocol/                # MCP protocol handler (JSON-RPC 2.0)
│   ├── tools/                   # Tool registry, definitions, and registration of MCP tools
│   │   ├── macros.lisp          # Validation macros (define-validated-tool, define-simple-tool)
│   │   ├── inspector-tools.lisp # Inspector tools (5)
│   │   ├── debugger-tools.lisp  # Debugger tools (6)
│   │   ├── repl-tools.lisp      # REPL tools (1)
│   │   ├── hot-reload-tools.lisp # Hot reload tools (2)
│   │   ├── profiler-tools.lisp  # Profiler tools (3)
│   │   ├── tracer-tools.lisp    # Tracer tools (3)
│   │   ├── thread-tools.lisp    # Thread tools (3)
│   │   ├── monitor-tools.lisp   # Monitor tools (4)
│   │   ├── logging-tools.lisp   # Logging tools (5)
│   │   ├── xref-tools.lisp      # XRef tools (5)
│   │   ├── security-tools.lisp  # Security tools (5)
│   │   ├── swank-tools.lisp     # Swank tools (21)
│   │   ├── unified-tools.lisp   # Unified REPL tools (23)
│   │   ├── registry.lisp        # Tool registry and load-all-tools
│   │   └── register-tools.lisp  # Main loader (calls load-all-tools)
│   ├── security/                # Approval workflow, audit logging
│   ├── sbcl/                    # SBCL-specific integration (eval, compile, threads)
│   ├── swank/                   # Swank (Slime) integration
│   ├── unified/                 # Unified REPL interface (Swank)
│   ├── debugger/                # Debugging tools (frames, restarts, breakpoints)
│   ├── inspector/               # Object introspection tools
│   ├── hot-reload/              # Live code modification
│   ├── profiler/                 # Performance profiling
│   ├── tracer/                  # Function tracing
│   ├── monitor/                 # Production monitoring
│   ├── logging/                 # log4cl integration
│   └── xref/                    # Cross-reference tools
│
├── tests/                       # Rove test suites
├── scripts/                     # run-http.sh, run-http-server.lisp, tutorial-run.lisp, debug-mcp-stdio.sh
├── examples/                    # MCP config examples + mcp-kilocode.json, test_client.py, cl_tron_client.py
├── reports/                     # Result and diagnostic reports (not tmp/)
├── prompts/                     # Workflow-specific prompts
│   ├── individual/              # Individual prompt definitions (9 files)
│   │   ├── discover-mcp.lisp    # How to fully use this MCP
│   │   ├── getting-started.lisp # Connection setup
│   │   ├── debugging-workflow.lisp
│   │   ├── hot-reload-workflow.lisp
│   │   ├── profiling-workflow.lisp
│   │   ├── token-optimization.lisp
│   │   ├── sbcl-debugging-expert.lisp
│   │   ├── hot-reload-specialist.lisp
│   │   └── performance-engineer.lisp
│   └── handler.lisp              # Prompt infrastructure (159 lines)
├── agents/                      # Specialized agent personas
├── docs/
│   ├── agents/                  # Modular agent documentation (this guide split into 6 files)
│   │   ├── index.md             # This overview
│   │   ├── getting-started.md   # First-time setup
│   │   ├── workflows.md         # Common workflows
│   │   ├── tool-reference.md    # Tool catalog
│   │   ├── conventions.md       # Coding standards
│   │   ├── transport-logging.md # Transport & logging
│   │   └── troubleshooting.md   # Common issues
│   ├── tools/                   # Tool documentation
│   └── plans/                   # Planning docs (e.g. swank-integration)
├── .cursor/                     # Cursor MCP configs
├── .vscode/                     # VS Code MCP configs
├── .kilocode/                   # Kilocode MCP configs
├── cl-tron-mcp.asd             # ASDF system definition
└── README.md                    # Project overview
```

## Build, Test, and Development Commands

### Building the System

```lisp
;; Load build via Quicklisp (recommended)
(ql:quickload :cl-tron-mcp)

;; Load via ASDF
(asdf:load-system :cl-tron-mcp)

;; Compile from source
(asdf:compile-system :cl-tron-mcp)

;; Force recompile
(asdf:compile-system :cl-tron-mcp :force t)
```

### Running Tests

```lisp
;; Run all tests via ASDF
(asdf:test-system :cl-tron-mcp)

;; Run via Rove
(ql:quickload :rove)
(ql:quickload :cl-tron-mcp)
(rove:run :cl-tron-mcp/tests)

;; Run specific test suite
(rove:run 'cl-tron-mcp/tests/core-test)
(rove:run 'cl-tron-mcp/tests/protocol-test)
(rove:run 'cl-tron-mcp/tests/security-test)

;; Run single test
(rove:run-test 'cl-tron-mcp/tests/core-test::version-test)
```

### Development Server

```lisp
;; Start MCP server (default: combined = stdio + HTTP on 4006)
(cl-tron-mcp/core:start-server)

;; Start MCP server (stdio only - e.g. when MCP client starts the server)
(cl-tron-mcp/core:start-server :transport :stdio-only)

;; Start MCP server (HTTP only via Hunchentoot; default port 4006)
(cl-tron-mcp/core:start-server :transport :http-only :port 4006)

;; Start MCP server (WebSocket transport)
(cl-tron-mcp/core:start-server :transport :websocket :port 23456)

;; Stop the server
(cl-tron-mcp/core:stop-server)

;; Check server state
(cl-tron-mcp/core:get-server-state)
```

### Testing with MCP Client

```bash
# Test with stdio (--stdio-only for pure stdio, or default combined).
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
# Or: echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh --stdio-only

# Quick verification
sbcl --non-interactive \
  --eval '(ql:quickload :cl-tron-mcp)' \
  --eval '(format t "Tools: ~d~%" (hash-table-count cl-tron-mcp/tools:*tool-registry*))'
```

## Security & Configuration Notes

### Security Model: User Approval Required

The MCP requires user approval for operations that can modify running code:

**Operations Requiring Approval:**

- `:eval` - Code execution
- `:compile-file` - Compilation
- `:modify-running-code` - Hot swapping
- `:terminate-thread` - Thread termination
- `:set-breakpoint` - Breakpoint insertion
- `:trace-function` - Function tracing
- `:modify-restarts` - Restart manipulation

**Tools Requiring Approval:**

- `repl_eval`
- `code_compile_string`
- `reload_system`
- `profile_start`
- `profile_stop`
- `trace_function`
- `swank_eval`, `swank_compile`, `swank_abort`, `swank_interrupt`
- `repl_eval`, `repl_compile`

### Approval Workflow (server-enforced)

- **Timeout:** 300 seconds (user can be busy elsewhere).
- **Two classes:** Tools with `approvalLevel: "user"` require human approval; `"none"` = auto-run (MCP/agent sufficient).
- **Flow:** When a protected tool is invoked, the server returns `approval_required` with `request_id` and `message`. The client shows UI; the user approves or denies. The client calls **`approval/respond`** with `request_id` and `approved` (true/false). Then the client **re-invokes** the same tool with `approval_request_id` and `approved: true` to run it. Denial returns a **message** (e.g. "User denied approval. You can retry by invoking the tool again."). **Retry** = call the same tool again (new approval request).
- **Whitelist:** When the operation is whitelisted, the tool runs without prompting.

### Audit Logging

All operations are logged with:

- Timestamp
- Operation type
- User approval status
- Operation details (sanitized)
- Duration
- Result

## MCP Client Integration

### OpenCode Integration

Configure in `~/.config/opencode/opencode.json`:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "cl-tron-mcp": {
      "type": "local",
      "command": ["~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh"],
      "enabled": true
    }
  }
}
```

### Cursor Integration

1. Install the MCP extension for Cursor
2. Copy `.cursor/mcp.json` to `~/.cursor/mcp.json`

### VS Code Integration

1. Install the MCP extension for VS Code
2. Copy `.vscode/mcp.json` to `~/.vscode/mcp.json`

### Kilocode Integration

1. Copy `.kilocode/mcp.json` to appropriate config location (or use `examples/kilocode-mcp.json.example` with your path).
2. **If Kilocode does not connect:** See [docs/starting-the-mcp.md § Debugging Kilocode MCP](docs/starting-the-mcp.md#debugging-kilocode-mcp): prove Tron alone (stdio), run the exact Kilocode command, use one transport at a time, check Kilocode/VS Code output, and optional `./scripts/debug-mcp-stdio.sh`.

### Manual Test (Verify Server Works)

```bash
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
```

Or: `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh --stdio-only`

Expected response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": [],
    "serverInfo": { "name": "cl-tron-mcp", "version": "0.1.0" }
  }
}
```

## Cross-References

@docs/architecture.md
@docs/mcp-resources-prompts.md
@prompts/debugging-workflows.md
@prompts/hot-reload-development.md
@prompts/profiling-analysis.md
@prompts/production-monitoring.md
@agents/sbcl-debugging-expert.md
@agents/performance-engineer.md
@agents/hot-reload-specialist.md

## Summary

- **Build**: `(ql:quickload :cl-tron-mcp)`
- **Test**: `(asdf:test-system :cl-tron-mcp)`
- **Dev Server**: `(cl-tron-mcp/core:start-server)` (combined) or `:transport :stdio-only` / `:http-only`
- **Style**: Google CL guide + project conventions
- **Tests**: Rove in `tests/`, mirror source structure
- **Security**: User approval required for modifying operations
- **Docs**: See `docs/agents/` for detailed guides (modularized for token efficiency)
- **Tools**: 86 tools implemented across 14 categories
- **Transport**: Combined (default: stdio + HTTP), stdio-only, http-only; WebSocket (placeholder)
- **MCP Clients**: Verified working with OpenCode, Cursor, VS Code