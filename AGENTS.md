# SBCL Debugging MCP Repository Guidelines

This document provides guidelines for AI agents working on the SBCL Debugging MCP project - a Model Context Protocol server that enables deep debugging, introspection, profiling, and hot code reloading for SBCL Common Lisp applications.

## Quick Reference

**Core Development Loop:**

```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

**Tool Categories (81 tools total):**

| Category    | Purpose                  | Key Tools                                                  |
| ----------- | ----------------------- | ---------------------------------------------------------- |
| Inspector   | Object introspection     | `inspect_object`, `inspect_class`, `inspect_function`        |
| Debugger    | Debugging operations    | `debugger_frames`, `debugger_restarts`, `breakpoint_set`    |
| REPL        | Code evaluation         | `repl_eval`                                                |
| Hot Reload  | Live code modification  | `code_compile_string`, `reload_system`                     |
| Profiler    | Performance analysis    | `profile_start`, `profile_stop`                            |
| Tracer      | Function tracing        | `trace_function`, `trace_list`                             |
| Threads     | Thread management       | `thread_list`, `thread_inspect`, `thread_backtrace`        |
| Monitor     | Production monitoring   | `health_check`, `runtime_stats`                            |
| Logging     | Package logging         | `log_configure`, `log_info`                                |
| XRef        | Cross-reference         | `who_calls`, `who_references`, `list_callees`              |
| Security    | Approval whitelist      | `whitelist_add`, `whitelist_status`                        |
| Swank       | Slime integration       | `swank_connect`, `swank_eval`, `swank_threads`             |
| nrepl       | Sly/CIDER integration   | `nrepl_connect`, `nrepl_eval`, `nrepl_sessions`           |
| Unified     | Auto-detect REPL       | `repl_connect`, `repl_eval`, `repl_threads`                |

## Cross-References

@prompts/debugging-workflows.md
@prompts/hot-reload-development.md
@prompts/profiling-analysis.md
@prompts/production-monitoring.md
@agents/sbcl-debugging-expert.md
@agents/performance-engineer.md
@agents/hot-reload-specialist.md

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

;; Start MCP server (HTTP transport on port 8080)
(cl-tron-mcp/core:start-server :transport :http :port 8080)

;; Start MCP server (WebSocket transport)
(cl-tron-mcp/core:start-server :transport :websocket :port 8081)

;; Stop the server
(cl-tron-mcp/core:stop-server)

;; Check server state
(cl-tron-mcp/core:get-server-state)
```

### Testing with MCP Client

```bash
# Test with stdio transport (primary method)
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio)'

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
  sbcl --non-interactive \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
```

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

### Typical Development Session

1. **Explore**: Use tools to understand current state
2. **Experiment**: Test in REPL with `repl_eval`
3. **Persist**: Edit files with `code_compile_string`
4. **Verify**: Run tests and verify functionality
5. **Hot-Reload**: Apply changes without restart using `reload_system`

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

### Unified REPL Tools (12) - Auto-Detect

- `repl_connect` - Connect to any REPL (auto-detects Swank/nrepl)
- `repl_disconnect` - Disconnect from the current REPL
- `repl_status` - Check REPL connection status and type
- `repl_eval` - Evaluate Lisp code via the connected REPL (requires approval)
- `repl_compile` - Compile Lisp code via the connected REPL (requires approval)
- `repl_threads` - List all threads via the connected REPL
- `repl_abort` - Abort/interrupt evaluation via the connected REPL
- `repl_backtrace` - Get backtrace via the connected REPL
- `repl_inspect` - Inspect an object via the connected REPL
- `repl_describe` - Describe a symbol via the connected REPL
- `repl_completions` - Get symbol completions via the connected REPL
- `repl_doc` - Get documentation for a symbol via the connected REPL

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
     sbcl --non-interactive \
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
- **Tools**: 81 tools implemented across 14 categories
- **Transport**: Stdio (primary), HTTP (has issues), WebSocket (placeholder)
- **MCP Clients**: Verified working with OpenCode, Cursor, VS Code
