# CL-TRON-MCP

A Model Context Protocol (MCP) server for SBCL Common Lisp that enables deep debugging, introspection, profiling, and hot code reloading for SBCL applications.

## Overview

CL-TRON-MCP provides a comprehensive debugging and introspection toolkit for SBCL Common Lisp applications through the Model Context Protocol. It enables AI assistants and development tools to interact with running Lisp images for debugging, code analysis, and live modification.

### Key Features

- **Inspector Tools**: Introspect objects, slots, CLOS classes, functions, and packages
- **Debugger Tools**: Access stack frames, restarts, breakpoints, and stepping control
- **REPL Integration**: Evaluate Lisp code in the running image context
- **Hot Reload**: Compile and load code strings, reload ASDF systems
- **Profiler**: Deterministic profiling with report generation
- **Tracer**: Function tracing with conditional support
- **Thread Management**: List, inspect, and get backtraces for threads
- **Monitor**: Health checks, memory statistics, and system information
- **Logging**: log4cl integration for package-level logging
- **Cross-Reference**: Find function callers, callees, and symbol references
- **Approval Whitelist**: Automate with pattern-based approval bypass

### Statistics

- **43 tools** across 11 categories
- Rove test suite with 8 tests
- Full MCP protocol support (stdio, HTTP, WebSocket)

### Requirements

- SBCL 2.0.0 or later
- Quicklisp
- ASDF 3.0 or later

## Installation

### Quicklisp (Recommended)

```lisp
;; Install via Quicklisp (if not already installed)
(ql:quickload :cl-tron-mcp)

;; Verify installation
(cl-tron-mcp:health-check)
;; => (:STATUS :HEALTHY ...)
```

### From Source

```bash
# Clone the repository
git clone https://github.com/yourusername/cl-tron-mcp.git
cd cl-tron-mcp

# Set up Quicklisp to find the local project
# Add to your ~/quicklisp/local-projects/ or use QL-PACKAGE-LOCAL-PROJECTS

# Load in SBCL
sbcl --eval '(ql:quickload :cl-tron-mcp)'
```

### Dependencies

CL-TRON-MCP automatically pulls dependencies via Quicklisp:

- `closer-mop` - CLOS MOP support
- `bordeaux-threads` - Portable threading
- `local-time` - Timestamp handling
- `rove` - Testing framework
- `hunchentoot` - HTTP transport (optional)

## Usage

### Starting the Server

#### Stdio Transport (Primary)

```lisp
;; Start MCP server with stdio transport
(cl-tron-mcp:start-server :transport :stdio)
```

#### HTTP Transport

```lisp
;; Start MCP server on port 8080
(cl-tron-mcp:start-server :transport :http :port 8080)
```

#### WebSocket Transport

```lisp
;; Start MCP server with WebSocket on port 8081
(cl-tron-mcp:start-server :transport :websocket :port 8081)
```

### Available Tools

#### Inspector Tools

**inspect_object** - Inspect an object by ID

```json
{
  "tool": "inspect_object",
  "arguments": {
    "objectId": "obj-123",
    "maxDepth": 3
  }
}
```

**inspect_slot** - Get or set a slot value on an object

```json
{
  "tool": "inspect_slot",
  "arguments": {
    "objectId": "obj-123",
    "slotName": "name",
    "value": "new-value"
  }
}
```

**inspect_class** - Inspect a CLOS class definition

```json
{
  "tool": "inspect_class",
  "arguments": {
    "className": "MY-CLASS"
  }
}
```

**inspect_function** - Inspect a function definition

```json
{
  "tool": "inspect_function",
  "arguments": {
    "symbolName": "MY-FUNCTION"
  }
}
```

**inspect_package** - Inspect a package and list its contents

```json
{
  "tool": "inspect_package",
  "arguments": {
    "packageName": "MY-PACKAGE"
  }
}
```

#### Debugger Tools

**debugger_frames** - Get debugger stack frames

```json
{
  "tool": "debugger_frames",
  "arguments": {
    "thread": null,
    "start": 0,
    "end": 20
  }
}
```

**debugger_restarts** - List available debugger restarts

```json
{
  "tool": "debugger_restarts",
  "arguments": {}
}
```

**breakpoint_set** - Set a breakpoint on a function

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "functionName": "MY-FUNCTION",
    "condition": null,
    "hitCount": 0
  }
}
```

**breakpoint_remove** - Remove a breakpoint by ID

```json
{
  "tool": "breakpoint_remove",
  "arguments": {
    "breakpointId": 1
  }
}
```

**breakpoint_list** - List all active breakpoints

```json
{
  "tool": "breakpoint_list",
  "arguments": {}
}
```

**step_frame** - Step execution in a frame

```json
{
  "tool": "step_frame",
  "arguments": {
    "frame": 0,
    "mode": "into"
  }
}
```

#### REPL Tools

**repl_eval** - Evaluate Lisp code (requires approval)

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(format t \"Hello, World!~%\")",
    "package": "CL-USER"
  }
}
```

#### Hot Reload Tools

**code_compile_string** - Compile and load Lisp code (requires approval)

```json
{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(defun hello () (format t \"Hello!~%))",
    "filename": "hello.lisp"
  }
}
```

**reload_system** - Reload ASDF system (requires approval)

```json
{
  "tool": "reload_system",
  "arguments": {
    "systemName": "MY-SYSTEM",
    "force": true
  }
}
```

#### Profiler Tools

**profile_start** - Start deterministic profiling (requires approval)

```json
{
  "tool": "profile_start",
  "arguments": {}
}
```

**profile_stop** - Stop profiling (requires approval)

```json
{
  "tool": "profile_stop",
  "arguments": {}
}
```

**profile_report** - Get profiling report

```json
{
  "tool": "profile_report",
  "arguments": {
    "format": "flat"
  }
}
```

#### Tracer Tools

**trace_function** - Add trace to a function (requires approval)

```json
{
  "tool": "trace_function",
  "arguments": {
    "functionName": "MY-FUNCTION",
    "condition": null,
    "hitCount": null
  }
}
```

**trace_remove** - Remove trace from a function (requires approval)

```json
{
  "tool": "trace_remove",
  "arguments": {
    "functionName": "MY-FUNCTION"
  }
}
```

**trace_list** - List all traced functions

```json
{
  "tool": "trace_list",
  "arguments": {}
}
```

#### Thread Tools

**thread_list** - List all threads with their status

```json
{
  "tool": "thread_list",
  "arguments": {}
}
```

**thread_inspect** - Get detailed information about a thread

```json
{
  "tool": "thread_inspect",
  "arguments": {
    "threadId": "main thread"
  }
}
```

**thread_backtrace** - Get backtrace for a specific thread

```json
{
  "tool": "thread_backtrace",
  "arguments": {
    "threadId": "main thread"
  }
}
```

#### Monitor Tools

**health_check** - Basic health check for the MCP server

```json
{
  "tool": "health_check",
  "arguments": {}
}
```

**runtime_stats** - Get runtime statistics

```json
{
  "tool": "runtime_stats",
  "arguments": {}
}
```

**gc_run** - Force garbage collection

```json
{
  "tool": "gc_run",
  "arguments": {
    "generation": 0
  }
}
```

**system_info** - Get comprehensive system information

```json
{
  "tool": "system_info",
  "arguments": {}
}
```

## Development

### Running Tests

```lisp
;; Run all tests
(asdf:test-system :cl-tron-mcp)

;; Or via Rove
(ql:quickload :rove)
(rove:run :cl-tron-mcp/tests)
```

### Project Structure

```
cl-tron-mcp/
├── src/
│   ├── core/              # Core infrastructure
│   ├── transport/         # Transport layer (stdio, http, websocket)
│   ├── protocol/          # MCP protocol handler
│   ├── tools/             # Tool registry
│   ├── security/          # Approval workflow, audit logging
│   ├── sbcl/              # SBCL integration
│   ├── debugger/          # Debugging tools
│   ├── inspector/         # Introspection tools
│   ├── repl/              # REPL
│   ├── hot-reload/        # Code reloading
│   ├── profiler/          # Profiling
│   ├── tracer/            # Tracing
│   └── monitor/           # Monitoring
├── tests/                 # Rove test suites
├── cl-tron-mcp.asd       # ASDF system definition
└── AGENTS.md             # AI agent guidelines
```

### Building

```lisp
;; Compile from source
(asdf:compile-system :cl-tron-mcp)

;; Force recompile
(asdf:compile-system :cl-tron-mcp :force t)
```

## Security

Operations that can modify running code require user approval:

- `:eval` - Code execution
- `:compile-file` - Compilation
- `:modify-running-code` - Hot swapping
- `:terminate-thread` - Thread termination
- `:set-breakpoint` - Breakpoint insertion
- `:trace-function` - Function tracing

Configure approval timeouts and behavior through the security module.

### Approval Whitelist

Operations can bypass approval for AI agent automation using the whitelist:

```json
{
  "tool": "whitelist_add",
  "arguments": {
    "operation": "eval",
    "pattern": "test-*"
  }
}
```

Whitelist management tools:
- `whitelist_add` - Add pattern to whitelist
- `whitelist_remove` - Remove pattern from whitelist
- `whitelist_clear` - Clear whitelist
- `whitelist_enable` - Enable/disable whitelist
- `whitelist_status` - Check whitelist status

## Logging Tools

**log_configure** - Configure logging level for a package

```json
{
  "tool": "log_configure",
  "arguments": {
    "level": "debug",
    "package": "my-package"
  }
}
```

**log_info**, **log_debug**, **log_warn**, **log_error** - Log messages

```json
{
  "tool": "log_info",
  "arguments": {
    "message": "Function started",
    "package": "my-package"
  }
}
```

## Cross-Reference Tools

**who_calls** - Find functions that call a symbol

```json
{
  "tool": "who_calls",
  "arguments": {
    "symbolName": "MY-FUNCTION"
  }
}
```

**list_callees** - List functions called by a symbol

```json
{
  "tool": "list_callees",
  "arguments": {
    "symbolName": "MY-FUNCTION"
  }
}
```

**who_references** - Find references to a symbol
**who_binds** - Find bindings of a symbol
**who_sets** - Find setq/makunbound of a symbol
**who_specializes** - Find methods that specialize on a symbol
**who_macroexpands** - Find macro expansions

## Tutorial

See `tutorial/` directory for step-by-step debugging tutorials:
- `tutorial/README.md` - Tutorial guide
- `tutorial/debugging-tutorial.lisp` - Lisp code examples
- `tutorial/tutorial.json` - JSON format scenarios

## Troubleshooting

| Symptom                       | Cause                         | Solution                                             |
| ----------------------------- | ----------------------------- | ---------------------------------------------------- |
| "Symbol not found"            | Package not loaded            | `(ql:quickload :cl-tron-mcp)`                        |
| "Approval timeout"            | User not responding           | Increase timeout or proceed without approval         |
| "Transport bind failed"       | Port in use                   | Use different port or kill conflicting process       |
| Tests failing                 | Stale FASL files              | `(asdf:compile-system :cl-tron-mcp :force t)`        |
| Debugger features unavailable | SBCL compiled without :sb-dbg | Rebuild SBCL with debugging or use default fallbacks |

## Contributing

1. Fork the repository
2. Create a feature branch
3. Implement changes following the `AGENTS.md` guidelines
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

CL-TRON-MCP is free software provided as is, with absolutely no warranty. It is mostly in the public domain; some portions are provided under BSD-style licenses. See the CREDITS and COPYING files in the distribution for more information.

## Resources

- [SBCL Documentation](http://www.sbcl.org/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [CLOS MOP Specification](https://clos-mop.hexstreamsoft.com/)
- [Bordeaux Threads](https://common-lisp.net/project/bordeaux-threads/)
