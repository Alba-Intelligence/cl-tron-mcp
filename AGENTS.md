# SBCL Debugging MCP Repository Guidelines

This document provides guidelines for AI agents working on the SBCL Debugging MCP project - a Model Context Protocol server that enables deep debugging, introspection, profiling, and hot code reloading for SBCL Common Lisp applications.

## Quick Reference

**Core Development Loop:**

```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

**Tool Categories (92 tools total):**

| Category    | Purpose                  | Key Tools                                                  |
| ----------- | ----------------------- | ---------------------------------------------------------- |
| Inspector   | Object introspection     | `inspect_object`, `inspect_class`, `inspect_function`        |
| Debugger    | Debugging operations    | `debugger_frames`, `debugger_restarts`, `breakpoint_set`    |
| REPL        | Code evaluation         | `repl_eval`, `repl_frame_locals`                           |
| Hot Reload  | Live code modification  | `code_compile_string`, `reload_system`                     |
| Profiler    | Performance analysis    | `profile_start`, `profile_stop`                            |
| Tracer      | Function tracing        | `trace_function`, `trace_list`                             |
| Threads     | Thread management       | `thread_list`, `thread_inspect`, `thread_backtrace`        |
| Monitor     | Production monitoring   | `health_check`, `runtime_stats`                            |
| Logging     | Package logging         | `log_configure`, `log_info`                                |
| XRef        | Cross-reference         | `who_calls`, `who_references`, `list_callees`              |
| Security    | Approval whitelist      | `whitelist_add`, `whitelist_status`                        |
| Swank       | Slime integration       | `swank_connect`, `swank_eval`, `swank_backtrace`, `swank_step` |
| nrepl       | Sly/CIDER integration   | `nrepl_connect`, `nrepl_eval`, `nrepl_sessions`           |
| Unified     | Auto-detect REPL        | `repl_connect`, `repl_eval`, `repl_step`, `repl_continue`  |

## Cross-References

@docs/architecture.md
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
- **Do not write to stdout** except the single JSON response line per request in `send-message-via-stdio`. No banners, no `[MCP]` messages, no notifications, no SBCL startup text on stdout.
- **All server activity must be logged via log4cl** (see below), not `format t` or `*standard-output*`. For stdio transport, log4cl is configured to write to stderr via `ensure-log-to-stream(*error-output*)`.

### Logging: use log4cl, not *error-output*

- **MCP activity (server start/stop, transport start, notifications, errors) must be logged through the `cl-tron-mcp/logging` API** (`log-info`, `log-warn`, `log-error`), not by writing directly to `*error-output*`. This keeps behaviour consistent and allows log4cl to route output (e.g. to stderr for stdio).
- When starting with `:stdio` transport, the server calls `ensure-log-to-stream(*error-output*)` so log4cl writes to stderr and stdout stays clean.

### SBCL startup (stdio)

- When launching SBCL for stdio transport (e.g. in `start-mcp.sh`), use **`--noinform`** so the SBCL banner is not printed to stdout. Otherwise the first line the client sees is not JSON and the handshake fails.
- In `start-mcp.sh`, the stdio branch uses `--noinform` and all pre-exec `echo` output is redirected to stderr (`>&2`) so no script banner appears on stdout.

### Reports

- **Result and diagnostic reports** (e.g. from diagnostic runs, test result summaries) must be stored in **`reports/`**, not in `tmp/`. See Project Structure.

## Recommended Workflow: One Long-Running Lisp Session

For the MCP to interact with Swank (or nrepl) the same way a user in Slime would—see output, debugger state, step, move frames, invoke restarts, inspect, compile—use a **single long-running Lisp session** that the user (or automation) starts and keeps running.

### Two processes

1. **Lisp session (Swank or nrepl)**  
   The user starts one SBCL (or other Lisp) with Swank (or nrepl) and leaves it running. All code loading and execution (by the user or by the MCP) happens in this process. The debugger runs here; Slime/Sly/Emacs can attach to the same session.

2. **MCP server**  
   Started by the MCP client (Cursor, Kilocode, Opencode) via `start-mcp.sh` or equivalent. It runs in a separate process and connects to the Lisp session as a **Swank (or nrepl) client**. The MCP then uses Swank facilities (eval, backtrace, restarts, stepping, inspect, etc.) over the protocol—the same way Slime does.

### Agent workflow

1. User starts the Lisp session with Swank (e.g. `(swank:create-server :port 4005)`) or nrepl (e.g. `(nrepl:start-server :port 8675)` using cl-nrepl).
2. User (or client) starts the MCP server; the agent connects to the Lisp session via `repl_connect` / `swank_connect` / `nrepl_connect` (or the client config is set so the MCP connects on startup).
3. The agent uses `repl_eval`, `repl_backtrace`, `repl_inspect`, and related tools to load code, run it, see output and debugger state, step, move frames, invoke restarts, and fix code—all through the connected session. No second REPL; one session, MCP as a client of it.

See **docs/architecture.md** and **README.md** (Swank Integration / Recommended setup) for step-by-step setup and tool usage.

## Project Structure & Module Organization

```
cl-tron-mcp/
├── src/
│   ├── core/                    # Core infrastructure (config, utils, version, server)
│   ├── transport/              # Transport layer (stdio, http, websocket)
│   ├── protocol/                # MCP protocol handler (JSON-RPC 2.0)
│   ├── tools/                   # Tool registry and definitions
│   ├── security/                # Approval workflow, audit logging
│   ├── sbcl/                    # SBCL-specific integration (eval, compile, threads)
│   ├── swank/                   # Swank (Slime) integration
│   ├── nrepl/                   # nrepl (Sly, CIDER) integration
│   ├── unified/                 # Unified REPL interface (auto-detect)
│   ├── debugger/                # Debugging tools (frames, restarts, breakpoints)
│   ├── inspector/               # Object introspection tools
│   ├── hot-reload/              # Live code modification
│   ├── profiler/                 # Performance profiling
│   ├── tracer/                  # Function tracing
│   ├── monitor/                 # Production monitoring
│   ├── logging/                 # log4cl integration
│   ├── xref/                    # Cross-reference tools
│   └── tools/                   # Tool registration
│
├── tests/                       # Rove test suites
├── reports/                     # Result and diagnostic reports (not tmp/)
├── prompts/                     # Workflow-specific prompts
├── agents/                      # Specialized agent personas
├── docs/
│   └── tools/                   # Tool documentation
├── .cursor/                     # Cursor MCP configs
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
;; Start MCP server (stdio transport - primary for AI agents)
(cl-tron-mcp/core:start-server :transport :stdio)

;; Start MCP server (HTTP transport on port 12345)
(cl-tron-mcp/core:start-server :transport :http :port 12345)

;; Start MCP server (WebSocket transport)
(cl-tron-mcp/core:start-server :transport :websocket :port 23456)

;; Stop the server
(cl-tron-mcp/core:stop-server)

;; Check server state
(cl-tron-mcp/core:get-server-state)
```

### Testing with MCP Client

```bash
# Test with stdio transport (primary method). Use --noinform so stdout = JSON only.
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
# Or: echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh

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
| Package           | `:cl-tron-mcp/debugger` | Package names use dash-separated words  |
| Functions         | `get-backtrace`         | Lower-case lisp-case                   |
| Predicates        | `breakpoint-active-p`    | End with `-p`                         |
| Constants         | `+max-registry-size+`    | Surrounded by `+`                     |
| Special variables | `*current-thread*`       | Surrounded by `*`                     |
| Condition types   | `evaluation-timeout`     | Lower-case, dash-separated             |

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
- `nrepl_eval`, `nrepl_compile`
- `repl_eval`, `repl_compile`

### Approval Workflow

```lisp
;; Request approval for operation
(let ((request (cl-tron-mcp:request-approval
                 :modify-running-code
                 (list :function "compute-data"
                       :file "src/core.lisp"))))
  ;; Wait for user response or timeout
  (when (eq (cl-tron-mcp:approval-response request) :approved)
    ;; Proceed with operation
    ))
```

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
      "command": ["/path/to/cl-tron-mcp/start-mcp.sh"],
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

1. Copy `.kilocode/mcp.json` to appropriate config location

### Manual Test (Verify Server Works)

```bash
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
```
Or: `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh`

Expected response:
```json
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":[],"serverInfo":{"name":"cl-tron-mcp","version":"0.1.0"}}}
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
(cl-tron-mcp/core:start-server :transport :stdio)
```

**First run with MCP clients:** `start-mcp.sh` and the direct SBCL configs (e.g. `.cursor/mcp-direct.json`, `.kilocode/run-mcp.sh`) set `*compile-verbose*` and `*load-verbose*` to nil so compilation does not pollute stdout. If a client still fails on first start, precompile once in a REPL with `(ql:quickload :cl-tron-mcp)` then restart the MCP server.

### Typical Development Session

When the MCP is connected to a long-running Lisp session (Swank/nrepl):

1. **Explore**: Use tools to understand current state (`repl_eval`, `repl_backtrace`, `repl_inspect`, etc.).
2. **Experiment**: Test in the connected REPL with `repl_eval`.
3. **Persist**: Edit files and compile with `code_compile_string` or `repl_compile`.
4. **Verify**: Run tests in the session (e.g. `repl_eval` with `(asdf:test-system :cl-tron-mcp)`).
5. **Debug**: Use Swank facilities via MCP tools—backtrace, restarts, step, frame up/down, inspect—the same way a user would in Slime.

Without a connected REPL, tools that require it (e.g. `repl_eval`) return "Not connected to any REPL"; use `repl_connect` or `swank_connect` / `nrepl_connect` first. See Recommended Workflow above and docs/architecture.md.

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

### nrepl Tools (14) - Sly/CIDER Integration

- `nrepl_connect` - Connect to nrepl server
- `nrepl_disconnect` - Disconnect from nrepl server
- `nrepl_status` - Check nrepl connection status
- `nrepl_eval` - Evaluate Lisp code via nrepl (requires approval)
- `nrepl_compile` - Compile Lisp code via nrepl (requires approval)
- `nrepl_sessions` - List all nrepl sessions
- `nrepl_close_session` - Close an nrepl session
- `nrepl_threads` - List all threads via nrepl
- `nrepl_interrupt` - Interrupt evaluation via nrepl
- `nrepl_backtrace` - Get backtrace via nrepl
- `nrepl_inspect` - Inspect an object via nrepl
- `nrepl_describe` - Describe a symbol via nrepl
- `nrepl_doc` - Get documentation for a symbol
- `nrepl_completions` - Get symbol completions

### Unified REPL Tools (23) - Auto-Detect

**Connection:**
- `repl_connect` - Connect to any REPL (auto-detects Swank/nrepl)
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

| Symptom                            | Cause                                      | Solution                                                      |
| ---------------------------------- | ------------------------------------------ | ------------------------------------------------------------- |
| "Symbol not found"                 | Package not loaded                         | `(ql:quickload :cl-tron-mcp)`                                 |
| "Approval timeout"                 | User not responding                        | Increase timeout or proceed without approval                   |
| "Transport bind failed"            | Port in use                                | Use different port or kill conflicting process                |
| Tests failing                      | Stale FASL files                           | `(asdf:compile-system :cl-tron-mcp :force t)`                 |
| HTTP transport issues              | Thread handling problems                   | Use stdio transport for production                            |
| Debugger features unavailable      | SBCL compiled without :sb-dbg              | Rebuild SBCL with debugging or use default fallbacks           |
| MCP client shows "failed"          | Wrong JSON key case                        | Ensure responses use lowercase keys (`jsonrpc`, `id`, `result`) |
| No response to requests            | Thread crash or buffering                  | Use stdio transport; ensure `force-output` is called           |
| "Package not found" error          | Quicklisp not loaded                       | Add `(ql:quickload :cl-tron-mcp)` before starting server      |
| "not a function" error            | Case sensitivity in symbol                 | Use uppercase: `CL:CAR` not `cl:car`                         |

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
       --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
   ```

## Summary

- **Build**: `(ql:quickload :cl-tron-mcp)`
- **Test**: `(asdf:test-system :cl-tron-mcp)`
- **Dev Server**: `(cl-tron-mcp/core:start-server :transport :stdio)`
- **Style**: Google CL guide + project conventions
- **Tests**: Rove in `tests/`, mirror source structure
- **Security**: User approval required for modifying operations
- **Docs**: See `@prompts/` and `docs/tools/` for detailed guides
- **Tools**: 92 tools implemented across 14 categories
- **Transport**: Stdio (primary), HTTP (has issues), WebSocket (placeholder)
- **MCP Clients**: Verified working with OpenCode, Cursor, VS Code
