# Conventions

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