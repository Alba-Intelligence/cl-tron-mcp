# SBCL Debugging MCP Repository Guidelines

This document provides guidelines for AI agents working on the SBCL Debugging MCP project - a Model Context Protocol server that enables deep debugging, introspection, profiling, and hot code reloading for SBCL Common Lisp applications.

## Quick Reference

**Core Development Loop:**
```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

**Tool Categories:**
| Category | Purpose | Key Tools |
|----------|---------|-----------|
| Inspector | Object introspection | `inspect_object`, `inspect_slot`, `inspect_class` |
| Debugger | Debugging operations | `debugger_frames`, `debugger_restarts`, `breakpoint_set` |
| REPL | Code evaluation | `repl_eval`, `repl_compile_and_load` |
| Hot Reload | Live code modification | `code_compile_string`, `code_reload_package` |
| Profiler | Performance analysis | `profile_start`, `sprof_start`, `profile_flamegraph` |
| Tracer | Function tracing | `trace_function`, `trace_get_entries` |
| Threads | Thread management | `thread_list`, `thread_debug`, `thread_kill` |
| Monitor | Production monitoring | `memory_stats`, `runtime_stats`, `health_check` |

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
│   ├── core/                    # Core infrastructure (config, utils, version)
│   ├── transport/              # Transport layer (stdio, http, websocket)
│   ├── protocol/                # MCP protocol handler
│   ├── tools/                   # Tool registry and definitions
│   ├── security/                # Approval workflow, audit logging
│   ├── sbcl/                    # SBCL-specific integration (eval, compile, threads)
│   └── debugger/                # Debugging tools (frames, restarts, breakpoints)
│
├── docs/
│   ├── tools/                   # Tool documentation (inspector.md, debugger.md, etc.)
│   ├── architecture.md          # System architecture
│   ├── protocol.md              # MCP protocol specification
│   ├── security.md              # Approval workflow documentation
│   ├── deployment.md            # Production deployment guide
│   └── examples/                # Usage examples
│
├── prompts/                     # Workflow-specific prompts
├── agents/                      # Specialized agent personas
├── tests/                       # Rove test suites
├── scripts/                     # Build and run scripts
│
├── cl-tron-mcp.asd             # ASDF system definition
└── README.md                   # Project overview
```

**Module Organization:**
- Each `src/` subdirectory is a separate package
- Tests mirror source file names under `tests/` with `-test` suffix
- New packages require explicit export lists
- Documentation files correspond to source modules

## Build, Test, and Development Commands

### Building the System

```l andisp
;; Load build via ASDF
(asdf:load-system :cl-tron-mcp)

;; Compile from source
(asdf:compile-system :cl-tron-mcp)

;; Force recompile
(asdf:compile-system :cl-tron-mcp :force t)
```

### Running Tests

```lisp
;; Run all tests
(asdf:test-system :cl-tron-mcp)

;; Run specific test suite
(rove:run 'cl-tron-mcp/tests/protocol-test)
(rove:run 'cl-tron-mcp/tests/debugger-test)
(rove:run 'cl-tron-mcp/tests/security-test)

;; Run single test
(rove:run-test 'cl-tron-mcp/tests/debugger-test::breakpoint-lifecycle)
```

### Development Server

```lisp
;; Start MCP server (stdio transport)
(cl-tron-mcp:start-server :transport :stdio)

;; Start MCP server (HTTP transport on port 8080)
(cl-tron-mcp:start-server :transport :http :port 8080)

;; Start MCP server (WebSocket transport)
(cl-tron-mcp:start-server :transport :websocket :port 8081)
```

### Testing with MCP Client

```bash
# Test with stdio transport
sbcl --non-interactive --load cl-tron-mcp.asd --eval '(cl-tron-mcp:start-server :transport :stdio)'

# Test with MCP Inspector or custom client
```

## Coding Style & Naming Conventions

Follow the Google Common Lisp Style Guide with project-specific additions:

### Formatting
- 2-space indent, ≤100 columns, no tabs
- One blank line between top-level forms
- File header → `(in-package ...)` → file-specific `declaim`

### Naming Conventions

| Pattern | Example | Usage |
|---------|---------|-------|
| Package | `:cl-tron-mcp/debugger` | Package names use dash-separated words |
| Functions | `get-backtrace` | Lower-case lisp-case |
| Predicates | `breakpoint-active-p` | End with `-p` |
| Constants | `+max-registry-size+` | Surrounded by `+` |
| Special variables | `*current-thread*` | Surrounded by `*` |
| Condition types | `evaluation-timeout` | Lower-case, dash-separated |

### SBCL-Specific Patterns

```lisp
;; SBCL debug internals
(sb-di:frame-debug-function frame)
(sb-di:debug-var-valid-p debug-var)
(sb-debug:break-at-breakpoint code-location)

;; Thread operations (Bordeaux-threads abstraction)
(bt:make-thread #'(lambda () ...) :name "worker")
(bt:current-thread)
(bt:list-all-threads)
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
;; tests/debugger-test.lisp
(defpackage :cl-tron-mcp/tests/debugger-test
  (:use #:cl #:rove))

(in-package :cl-tron-mcp/tests/debugger-test)

(deftest breakpoint-lifecycle
  (testing "Breakpoint can be set, listed, and removed"
    (let ((id (cl-tron-mcp:breakpoint-set "test-fun")))
      (ok id)
      (ok (cl-tron-mcp:breakpoint-list))
      (ok (cl-tron-mcp:breakpoint-remove id)))))

(deftest frame-inspection
  (testing "Frame locals are accessible"
    (ok (cl-tron-mcp:debugger-frame-locals 0))))
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
(rove:run 'cl-tron-mcp/tests/debugger-test)

;; With stdout capture
(with-output-to-string (*standard-output*)
  (rove:run 'cl-tron-mcp/tests/debugger-test))
```

## Commit & Pull Request Guidelines

### Commit Message Format

```
[component]: [imperative summary]

[Optional body explaining the change]

[Optional footer with issue references]

Components:
- transport: Transport layer changes
- protocol: Protocol handler changes  
- tool: Tool implementation or update
- debugger: Debugging tools
- profiler: Profiling tools
- hot-reload: Code reloading tools
- security: Approval workflow changes
- doc: Documentation updates
- test: Test additions or changes
- refactor: Code improvements
```

### Examples

```
transport: Add WebSocket transport support

- Implement WebSocket upgrade handling
- Add heartbeat mechanism for connection liveness
- Support streaming debug events via WebSocket

Refs: #12
```

```
debugger: Implement conditional breakpoints

- Add condition evaluation via SBCL eval
- Support hit-count breakpoints
- Add breakpoint enabled/disabled state

Closes: #8
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

## Development Workflow

### First-Time Setup

```lisp
;; 1. Set project root (for file operations)
(cl-tron-mcp:fs-set-project-root ".")

;; 2. Load the system
(ql:quickload :cl-tron-mcp)

;; 3. Start development server
(cl-tron-mcp:start-server :transport :stdio)
```

### Typical Development Session

1. **Explore**: Use tools to understand current state
2. **Experiment**: Test in REPL with `repl_eval`
3. **Persist**: Edit files with `lisp-edit-form` or `code_compile_string`
4. **Verify**: Run tests and verify functionality
5. **Hot-Reload**: Apply changes without restart using `code_reload_package`

### Tool Usage Order

```
SEARCH → READ → UNDERSTAND → EDIT → COMPILE → VERIFY
   ↓        ↓          ↓         ↓         ↓         ↓
clgrep   lisp-read   inspect   lisp-edit code_    tests
         file        object    form      compile
```

## Troubleshooting

### Common Issues

| Symptom | Cause | Solution |
|---------|-------|----------|
| "Symbol not found" | Package not loaded | `(ql:quickload :cl-tron-mcp)` |
| "Approval timeout" | User not responding | Increase timeout or proceed without approval |
| "Transport bind failed" | Port in use | Use different port or kill conflicting process |
| Tests failing | Stale FASL files | `(asdf:compile-system :cl-tron-mcp :force t)` |

### Getting Help

1. Check `@prompts/debugging-workflows.md` for debugging guidance
2. Review `@agents/sbcl-debugging-expert.md` for debugging strategies
3. Reference `docs/tools/` for specific tool documentation
4. Examine test files for usage examples

## Summary

- **Build**: `asdf:compile-system :cl-tron-mcp`
- **Test**: `asdf:test-system :cl-tron-mcp`
- **Dev Server**: `(cl-tron-mcp:start-server :transport :stdio)`
- **Style**: Google CL guide + project conventions
- **Tests**: Rove in `tests/`, mirror source structure
- **Security**: User approval required for modifying operations
- **Docs**: See `@prompts/` and `docs/tools/` for detailed guides
