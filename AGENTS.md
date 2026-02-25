# SBCL Debugging MCP Repository Guidelines

This document provides guidelines for AI agents working on the SBCL Debugging MCP project - a Model Context Protocol server that enables deep debugging, introspection, profiling, and hot code reloading for SBCL Common Lisp applications.

## How to Use This MCP (Fully Discoverable — No User Explanation Needed)

An AI agent can learn how to fully use this MCP without any user explanation. Use one of these entry points:

- **If you prefer a short recipe:** Call **prompts/get** with name **`discover-mcp`**. It returns the exact ordered steps (resources/list → resources/read AGENTS.md → prompts/list → prompts/get getting-started → tools/list).
- **If you prefer to read:** Call **resources/list**, then **resources/read** with uri **`AGENTS.md`**. This document (plus the other listed resources) explains connection, tools, workflows, and conventions.

After following that path you have everything needed to connect to Swank, evaluate code, debug, inspect, profile, and hot-reload. The MCP is fully discoverable via standard MCP methods: `resources/list`, `resources/read`, `prompts/list`, `prompts/get`, `tools/list`.

## Quick Start for AI Agents

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

### Essential Pattern: One Long-Running Session

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

### First Steps (Do This Now)

We recommend using port 4006 for the Swank server. That way 4005 is available for the user's Slime.

### Common Workflows

| Task               | Tools                                                             | Pattern                              |
| ------------------ | ----------------------------------------------------------------- | ------------------------------------ |
| **Evaluate code**  | `swank_eval`                                                      | Send code string, get result         |
| **Debug error**    | `swank_eval` → error → `swank_backtrace` → `swank_invoke_restart` | Trigger error, see frames, fix       |
| **Inspect object** | `inspect_object`, `inspect_class`                                 | Get object ID, inspect slots         |
| **Profile code**   | `profile_start` → run code → `profile_stop` → `profile_report`    | Measure, analyze                     |
| **Hot-fix bug**    | `swank_compile` or `swank_eval` with `defun`                      | Redefine function in running session |
| **Find callers**   | `who_calls`                                                       | Cross-reference analysis             |

### When You See an Error

1. Don't panic - the debugger is active
2. Use `swank_backtrace` to see stack frames
3. Use `swank_get_restarts` to see options
4. Either fix the code and `swank_eval` again, or `swank_invoke_restart` to abort
5. The session persists - state is not lost

### Key Insights

- **All tools use `&key` args with underscore names**: `symbol_name`, not `symbol-name`
- **Results are plists**: Access with `(getf result :key)`
- **Port 4005 = Swank** (Slime/Portacle/Sly)
- **Use `tmp/` folder**, never `/tmp`

### Lisp Implementation Support

- **Primary:** Tested with **SBCL**; Swank integration and debugger features are developed against SBCL.
- **ECL:** The MCP server can run under **ECL** as well. Lisp selection in `start-mcp.sh`: **CLI** (`--use-sbcl` / `--use-ecl`) or **auto-detect** (sbcl, then ecl). Run `./start-mcp.sh --help` for full usage. For stdio, the script uses SBCL `--noinform` or ECL `-q` so stdout stays JSON-only.
- **Goal:** Any Common Lisp implementation should be able to use the MCP where possible; REPL connectivity is via Swank.

## Quick Reference

**Core Development Loop:**

```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

**MCP Protocol Methods:**

| Method           | Purpose                                                  |
| ---------------- | -------------------------------------------------------- |
| `resources/list` | List documentation files (AGENTS.md, README.md, docs/\*) |
| `resources/read` | Read a documentation file by URI                         |
| `prompts/list`   | List guided workflows                                    |
| `prompts/get`    | Get step-by-step workflow instructions                   |
| `tools/list`     | List available tools                                     |
| `tools/call`     | Invoke a tool                                            |

**Guided Workflows (prompts/get):**

| Prompt                | Purpose                                                                      |
| --------------------- | ---------------------------------------------------------------------------- |
| `discover-mcp`        | How to fully use this MCP without user explanation (ordered discovery steps) |
| `getting-started`     | How to connect to Swank and verify setup                                     |
| `debugging-workflow`  | Step-by-step error debugging                                                 |
| `hot-reload-workflow` | Live code modification without restart                                       |
| `profiling-workflow`  | Performance analysis workflow                                                |

**Tool Categories (86 tools total):**

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

## CRITICAL: Stdio Transport and Logging

When the server is launched by MCP clients (Cursor, Kilocode, Opencode) over stdio, the following rules are **mandatory** or clients will fail to parse the stream.

### Stdout purity (stdio transport)

- **MCP over stdio expects stdout to contain only newline-delimited JSON-RPC messages.** The client reads line-by-line and parses each line as JSON. Any other output on stdout breaks the protocol.
- **Do not write to stdout** except the single JSON response line per request in `send-message-via-stdio`. No banners, no `[MCP]` messages, no notifications, no SBCL startup text on stdout. Handlers return already-serialized JSON strings; the transport writes them as-is (no double-encoding) so clients receive a JSON object per line.
- **All server activity must be logged via log4cl** (see below), not `format t` or `*standard-output*`. For stdio transport, log4cl is configured to write to stderr via `ensure-log-to-stream(*error-output*)`.

### Logging: use log4cl, not _error-output_

- **MCP activity (server start/stop, transport start, notifications, errors) must be logged through the `cl-tron-mcp/logging` API** (`log-info`, `log-warn`, `log-error`), not by writing directly to `*error-output*`. This keeps behaviour consistent and allows log4cl to route output (e.g. to stderr for stdio).
- When starting with stdio (e.g. `:stdio-only` or `:combined`), the server calls `ensure-log-to-stream(*error-output*)` so log4cl writes to stderr and stdout stays clean.

### SBCL startup (stdio)

- When launching SBCL for stdio transport (e.g. in `start-mcp.sh`), use **`--noinform`** so the SBCL banner is not printed to stdout. Otherwise the first line the client sees is not JSON and the handshake fails.
- In `start-mcp.sh`, the stdio branch uses `--noinform` and all pre-exec `echo` output is redirected to stderr (`>&2`) so no script banner appears on stdout. When the transport loop exits (client disconnect), the script quits the Lisp process so the REPL never reads from stdin.

### Reports

- **Result and diagnostic reports** (e.g. from diagnostic runs, test result summaries) must be stored in **`reports/`**, not in `tmp/`. See Project Structure.

### HTTP and WebSocket Transport

- **Transport modes:** Default is **combined** (stdio + HTTP): `./start-mcp.sh` runs both. Use **`--stdio-only`** for stdio only (e.g. when the MCP client starts the server) or **`--http-only`** for HTTP only. Run `./start-mcp.sh --help` for options.
- **HTTP:** Implemented with **Hunchentoot**. HTTP port default is 4006 (to avoid Swank on 4005). Start with `./start-mcp.sh` (combined) or `./start-mcp.sh --http-only [--port 4006]`. Clients POST JSON-RPC to `http://127.0.0.1:PORT/rpc` or `/mcp`; GET endpoints include `/health`, `/lisply/ping-lisp`, `/lisply/tools/list`. Server log messages use log4cl (stderr).
- **WebSocket:** Placeholder only (no real server or message handling); `start-websocket-transport` prints a notice.

### MCP protocol compliance (stdio and HTTP)

- **Initialize:** `capabilities.resources.subscribe` is sent as boolean `false` (MCP spec; Jonathan encodes `nil` as `[]` otherwise).
- **Prompts:** `prompts/get` returns messages with `content` as an array of parts per MCP spec (e.g. `[{ "type": "text", "text": "..." }]`), with lowercase keys, so Cursor and other clients accept slash-command payloads.

## Recommended Workflow: One Long-Running Lisp Session

For the MCP to interact with Swank the same way a user in Slime would—see output, debugger state, step, move frames, invoke restarts, inspect, compile—use a **single long-running Lisp session** that the user (or automation) starts and keeps running.

### Two processes

1. **Lisp session (Swank)**
   The user starts one SBCL (or other Lisp) with Swank and leaves it running. All code loading and execution (by the user or by the MCP) happens in this process. The debugger runs here; Slime/Sly/Emacs can attach to the same session.

2. **MCP server**
   Started by the MCP client (Cursor, Kilocode, Opencode) via `start-mcp.sh` or equivalent. It runs in a separate process and connects to the Lisp session as a **Swank client**. The MCP then uses Swank facilities (eval, backtrace, restarts, stepping, inspect, etc.) over the protocol—the same way Slime does.

### Agent workflow

1. User starts the Lisp session with Swank (e.g. `(swank:create-server :port 4006)`).
2. User (or client) starts the MCP server; the agent connects to the Lisp session via `repl_connect` or `swank_connect` (or the client config is set so the MCP connects on startup).
3. The agent uses `repl_eval`, `repl_backtrace`, `repl_inspect`, and related tools to load code, run it, see output and debugger state, step, move frames, invoke restarts, and fix code—all through the connected session. No second REPL; one session, MCP as a client of it.

See **docs/architecture.md** and **README.md** (Swank Integration / Recommended setup) for step-by-step setup and tool usage.

### Unified vs Swank

- After connecting, use **unified `repl_*` tools only** (do not mix with `swank_*`) to reduce mental load.
- **Dedicated port for MCP:** Use a separate Swank port for MCP (e.g. Swank on 4006) so the user keeps 4005 for their editor. Example: `(swank:create-server :port 4006 :dont-close t)` for MCP; keep 4005 for Slime.

### Restarts

- Use **`repl_get_restarts`** and **`repl_invoke_restart`** (unified) instead of `swank_get_restarts` / `swank_invoke_restart` in normal workflows.

## Project Structure & Module Organization

```
cl-tron-mcp/
├── src/
│   ├── core/                    # Core infrastructure (config, utils, version, server)
│   ├── transport/              # Transport layer (stdio, http via Hunchentoot, websocket)
│   ├── protocol/                # MCP protocol handler (JSON-RPC 2.0)
│   ├── tools/                   # Tool registry, definitions, and registration of MCP tools
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
├── agents/                      # Specialized agent personas
├── docs/
│   ├── tools/                   # Tool documentation
│   └── plans/                   # Planning docs (e.g. swank-integration)
├── .cursor/                     # Cursor MCP configs (tilde expansion works; adjust path if needed)
├── .vscode/                     # VS Code MCP configs
├── .kilocode/                   # Kilocode MCP configs
├── cl-tron-mcp.asd             # ASDF system definition
└── README.md                    # Project overview
```

**Module Organization:**

- Each `src/` subdirectory is a separate package with `:cl-tron-mcp/<module>` naming
- Tests mirror source file names under `tests/` with `-test` suffix
- New packages require explicit export lists
- Documentation files correspond to source modules
- **Reports:** Result and diagnostic reports (e.g. from diagnostic runs, test result summaries) must be stored in `reports/`, not in `tmp/`; `tmp/` is for ephemeral files only

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

## Coding Style & Naming Conventions

Follow the Google Common Lisp Style Guide with project-specific additions:

### Formatting

- 2-space indent, ≤100 columns, no tabs
- One blank line between top-level forms
- File header → `(in-package ...)` → file-specific `declaim`

### Naming Conventions

| Pattern           | Example                 | Usage                                  |
| ----------------- | ----------------------- | -------------------------------------- |
| Package           | `:cl-tron-mcp/debugger` | Package names use dash-separated words |
| Functions         | `get-backtrace`         | Lower-case lisp-case                   |
| Predicates        | `breakpoint-active-p`   | End with `-p`                          |
| Constants         | `+max-registry-size+`   | Surrounded by `+`                      |
| Special variables | `*current-thread*`      | Surrounded by `*`                      |
| Condition types   | `evaluation-timeout`    | Lower-case, dash-separated             |

### SBCL-Specific Patterns

```lisp
;; SBCL debug internals (use #+sb-dbg feature guard)
#+sb-dbg
(sb-di:frame-debug-function frame)
#+sb-dbg
(sb-di:debug-var-valid-p debug-var)

;; Thread operations (Bordeaux-threads abstraction)
(bt:make-thread #'(lambda () ...) :name "worker")
(bt:current-thread)
(bt:list-all-threads)
```

### JSON-RPC Response Format

All JSON-RPC encoding and decoding uses **Jonathan** (Common Lisp JSON library: `jonathan:parse`, `jonathan:to-json`). Use lowercase keys for MCP compatibility.

**Critical**: MCP responses MUST use lowercase JSON keys:

```lisp
;; Correct - lowercase keys
(jonathan:to-json (list :|jsonrpc| "2.0" :|id| id :|result| result))

;; Incorrect - uppercase keys will break MCP clients
(jonathan:to-json (list :jsonrpc "2.0" :id id :result result))
```

For hash tables, use `"|key|"` syntax for lowercase output:

```lisp
(let ((h (make-hash-table)))
  (setf (gethash "|name|" h) "value")
  (jonathan:to-json h))
```

### Documentation Requirements

- All public functions require docstrings
- Document condition types and restarts in situ
- Tool handlers require parameter and return documentation
- Approval workflow steps must be documented

## Testing Guidelines

### Test Framework

- Use **Rove** for all tests
- Tests live in `tests/` with matching source file names
- ASDF `test-op` wires to Rove test suites

### Test Organization

```lisp
;; tests/core-test.lisp
(defpackage :cl-tron-mcp/tests/core-test
  (:use #:cl #:rove))

(in-package :cl-tron-mcp/tests/core-test)

(deftest version-test
  (testing "Version is defined"
    (ok (stringp cl-tron-mcp/core:*version*))))
```

### Test Coverage Requirements

- All tool handlers must have tests
- Error handling paths must have tests
- Approval workflow must have tests
- Transport layers must have tests
- Maintain >80% code coverage

### Running Tests During Development

```lisp
;; In REPL, run tests without leaving
(rove:run :cl-tron-mcp/tests)

;; With stdout capture
(with-output-to-string (*standard-output*)
  (rove:run :cl-tron-mcp/tests))
```

## Commit & Pull Request Guidelines

### Commit Message Format

```
[component]: [imperative summary]

[Optional body explaining the change]

[Optional footer with issue references]

Components:
- transport: Transport layer changes
- protocol: Protocol handler changes (JSON-RPC)
- tool: Tool implementation or update
- debugger: Debugging tools
- profiler: Profiling tools
- hot-reload: Code reloading tools
- security: Approval workflow changes
- doc: Documentation updates
- test: Test additions or changes
- refactor: Code improvements
- mcp-client: MCP client integration (Cursor, VS Code, etc.)
```

### Examples

```
mcp-client: Add Cursor and VS Code MCP configurations

- Add .cursor/mcp.json for Cursor IDE
- Add .vscode/mcp.json for VS Code
- Update README with integration instructions
- Fix JSON-RPC responses to use lowercase keys

Refs: #42
```

```
protocol: Fix JSON-RPC lowercase key issue

- Change :jsonrpc to :|jsonrpc| in all responses
- Change :id to :|id| in all responses
- Change :result to :|result| in all responses
- MCP clients require lowercase keys (jsonrpc, id, result)
```

### Pull Request Requirements

1. **Title**: Clear, concise summary
2. **Description**:
   - What changed and why
   - Tool definitions updated (attach JSON)
   - Test commands run
   - Known limitations
3. **Testing**:
   - `(asdf:test-system :cl-tron-mcp)` passes
   - Manual testing documented
   - MCP client verification (if applicable)
4. **Documentation**:
   - Updated tool docs if applicable
   - Examples added for new features

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

### Security Best Practices

- Treat `repl_eval` as trusted input only
- Prefer stdio transport for local development
- Secure HTTP/WebSocket ports in production
- Redact secrets before logging
- Validate all user inputs
- Use SBCL's safe reader options

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

## Development Workflow

### First-Time Setup

```lisp
;; Load the system
(ql:quickload :cl-tron-mcp)

;; Verify installation
(format t "Version: ~a~%" cl-tron-mcp/core:*version*)
(format t "Tools registered: ~d~%" (hash-table-count cl-tron-mcp/tools:*tool-registry*))

;; Start development server
(cl-tron-mcp/core:start-server :transport :stdio-only)
```

**First run with MCP clients:** `start-mcp.sh` and the direct SBCL configs (e.g. `.cursor/mcp-direct.json`, `.kilocode/run-mcp.sh`) set `*compile-verbose*` and `*load-verbose*` to nil so compilation does not pollute stdout. If a client still fails on first start, precompile once in a REPL with `(ql:quickload :cl-tron-mcp)` then restart the MCP server.

### Typical Development Session

When the MCP is connected to a long-running Lisp session (Swank):

1. **Explore**: Use tools to understand current state (`repl_eval`, `repl_backtrace`, `repl_inspect`, etc.).
2. **Experiment**: Test in the connected REPL with `repl_eval`.
3. **Persist**: Edit files and compile with `code_compile_string` or `repl_compile`.
4. **Verify**: Run tests in the session (e.g. `repl_eval` with `(asdf:test-system :cl-tron-mcp)`).
5. **Debug**: Use Swank facilities via MCP tools—backtrace, restarts, step, frame up/down, inspect—the same way a user would in Slime.

Without a connected REPL, tools that require it (e.g. `repl_eval`) return "Not connected to any REPL"; use `repl_connect` or `swank_connect` first. See Recommended Workflow above and docs/architecture.md.

### Tool Usage Order

```
SEARCH → READ → UNDERSTAND → EDIT → COMPILE → VERIFY
   ↓        ↓          ↓         ↓         ↓         ↓
clgrep   lisp-read   inspect   code_      compile   tests
                    object    compile    string
```

## Implemented Tools

### Inspector Tools (5)

- `inspect_object` - Inspect an object by ID
- `inspect_slot` - Get or set a slot value on an object
- `inspect_class` - Inspect a CLOS class definition
- `inspect_function` - Inspect a function definition
- `inspect_package` - Inspect a package and list its contents

### Debugger Tools (6)

- `debugger_frames` - Get debugger stack frames
- `debugger_restarts` - List available debugger restarts
- `breakpoint_set` - Set a breakpoint on a function (requires approval)
- `breakpoint_remove` - Remove a breakpoint by ID
- `breakpoint_list` - List all active breakpoints
- `step_frame` - Step execution in a frame

### REPL Tools (1)

- `repl_eval` - Evaluate Lisp code in REPL context (requires approval)

### Hot Reload Tools (2)

- `code_compile_string` - Compile and load Lisp code string (requires approval)
- `reload_system` - Reload ASDF system (requires approval)

### Profiler Tools (3)

- `profile_start` - Start deterministic profiling (requires approval)
- `profile_stop` - Stop profiling (requires approval)
- `profile_report` - Get profiling report

### Tracer Tools (3)

- `trace_function` - Add trace to a function (requires approval)
- `trace_remove` - Remove trace from a function (requires approval)
- `trace_list` - List all traced functions

### Thread Tools (3)

- `thread_list` - List all threads with their status
- `thread_inspect` - Get detailed information about a thread
- `thread_backtrace` - Get backtrace for a specific thread

### Monitor Tools (4)

- `health_check` - Basic health check for the MCP server
- `runtime_stats` - Get runtime statistics including memory and thread info
- `gc_run` - Force garbage collection
- `system_info` - Get comprehensive system information

### Logging Tools (5)

- `log_configure` - Configure logging level for a package
- `log_info` - Log an info message
- `log_debug` - Log a debug message
- `log_warn` - Log a warning message
- `log_error` - Log an error message

### Cross-Reference Tools (5)

- `who_calls` - Find functions that call a symbol
- `who_references` - Find references to a symbol
- `who_binds` - Find bindings of a symbol
- `who_sets` - Find setq/makunbound of a symbol
- `list_callees` - List functions called by a symbol

### Security/Approval Tools (5)

- `whitelist_add` - Add pattern to approval whitelist
- `whitelist_remove` - Remove pattern from approval whitelist
- `whitelist_clear` - Clear the approval whitelist
- `whitelist_enable` - Enable/disable the approval whitelist
- `whitelist_status` - Get current whitelist status

### Swank Tools (13) - Slime/Portacle Integration

- `swank_connect` - Connect to Swank server
- `swank_disconnect` - Disconnect from Swank
- `swank_status` - Get Swank connection status
- `swank_eval` - Evaluate Lisp code via Swank (requires approval)
- `swank_compile` - Compile Lisp code via Swank (requires approval)
- `swank_threads` - List all threads in Swank-connected SBCL
- `swank_abort` - Abort a specific thread (requires approval)
- `swank_interrupt` - Interrupt evaluation (requires approval)
- `swank_backtrace` - Get the current backtrace
- `swank_inspect` - Inspect an object via Swank
- `swank_describe` - Describe a symbol via Swank
- `swank_autodoc` - Get documentation for a symbol
- `swank_completions` - Get symbol completions via Swank

### Unified REPL Tools (23) - Swank

**Connection:**

- `repl_connect` - Connect to Swank REPL
- `repl_disconnect` - Disconnect from the current REPL
- `repl_status` - Check REPL connection status and type

**Evaluation:**

- `repl_eval` - Evaluate Lisp code via the connected REPL (requires approval)
- `repl_compile` - Compile Lisp code via the connected REPL (requires approval)

**Debugging:**

- `repl_backtrace` - Get backtrace via the connected REPL
- `repl_frame_locals` - Get local variables for a frame
- `repl_get_restarts` - Get available restarts
- `repl_invoke_restart` - Invoke a restart by index
- `repl_step` - Step into next expression
- `repl_next` - Step over next expression
- `repl_out` - Step out of current frame
- `repl_continue` - Continue execution from debugger

**Breakpoints:**

- `repl_set_breakpoint` - Set a breakpoint on a function (requires approval)
- `repl_remove_breakpoint` - Remove a breakpoint by ID
- `repl_list_breakpoints` - List all breakpoints
- `repl_toggle_breakpoint` - Toggle breakpoint enabled state

**Inspection:**

- `repl_inspect` - Inspect an object via the connected REPL
- `repl_describe` - Describe a symbol via the connected REPL
- `repl_completions` - Get symbol completions via the connected REPL
- `repl_doc` - Get documentation for a symbol via the connected REPL

**Threads:**

- `repl_threads` - List all threads via the connected REPL
- `repl_abort` - Abort/interrupt evaluation via the connected REPL

**Help:**

- `repl_help` - Get help on available unified REPL tools

## Troubleshooting

### Common Issues

| Symptom                       | Cause                         | Solution                                                        |
| ----------------------------- | ----------------------------- | --------------------------------------------------------------- |
| "Symbol not found"            | Package not loaded            | `(ql:quickload :cl-tron-mcp)`                                   |
| "Approval timeout"            | User not responding           | Increase timeout or proceed without approval                    |
| "Transport bind failed"       | Port in use                   | Use different port or kill conflicting process                  |
| Tests failing                 | Stale FASL files              | `(asdf:compile-system :cl-tron-mcp :force t)`                   |
| Stop HTTP server              | Process runs until stopped    | Ctrl+C in the terminal where the server is running              |
| Debugger features unavailable | SBCL compiled without :sb-dbg | Rebuild SBCL with debugging or use default fallbacks            |
| MCP client shows "failed"     | Wrong JSON key case           | Ensure responses use lowercase keys (`jsonrpc`, `id`, `result`) |
| No response to requests       | Thread crash or buffering     | Use stdio transport; ensure `force-output` is called            |
| "Package not found" error     | Quicklisp not loaded          | Add `(ql:quickload :cl-tron-mcp)` before starting server        |
| "not a function" error        | Case sensitivity in symbol    | Use uppercase: `CL:CAR` not `cl:car`                            |

### Getting Help

1. Check `@prompts/debugging-workflows.md` for debugging guidance
2. Review `@agents/sbcl-debugging-expert.md` for debugging strategies
3. Reference `docs/tools/` for specific tool documentation
4. Examine test files for usage examples
5. Test server manually to verify it works:
   ```bash
   echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
     sbcl --non-interactive --noinform \
       --eval '(ql:quickload :cl-tron-mcp :silent t)' \
       --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
   ```

## Summary

- **Build**: `(ql:quickload :cl-tron-mcp)`
- **Test**: `(asdf:test-system :cl-tron-mcp)`
- **Dev Server**: `(cl-tron-mcp/core:start-server)` (combined) or `:transport :stdio-only` / `:http-only`
- **Style**: Google CL guide + project conventions
- **Tests**: Rove in `tests/`, mirror source structure
- **Security**: User approval required for modifying operations
- **Docs**: See `@prompts/` and `docs/tools/` for detailed guides
- **Tools**: 86 tools implemented across 14 categories
- **Transport**: Combined (default: stdio + HTTP), stdio-only, http-only; WebSocket (placeholder)
- **MCP Clients**: Verified working with OpenCode, Cursor, VS Code
