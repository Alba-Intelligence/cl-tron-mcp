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
- **Logging**: log4cl integration for package-level and server-activity logging (stdout kept clean for stdio MCP)
- **Cross-Reference**: Find function callers, callees, and symbol references
- **Approval Whitelist**: Automate with pattern-based approval bypass
- **Unified REPL Interface**: Single API for Swank (Slime) and nrepl (Sly, CIDER) - auto-detects protocol

### Statistics

- **81 tools** across 14 categories
- Rove test suite with 20+ tests
- Full MCP protocol support (stdio, HTTP, WebSocket)
- Verified working with OpenCode, Cursor, and VS Code

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
(cl-tron-mcp/core:health-check)
;; => (:STATUS :HEALTHY :TOOLS 55 ...)
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
- `jonathan` - JSON handling
- `usocket` - Socket support for HTTP transport

## MCP Client Integration

CL-TRON-MCP works with any MCP-compatible client including **Opencode**, **Cursor**, **Claude Code**, and **VS Code**.

### Python Client Library

```python
from cl_tron_client import CLTronClient

with CLTronClient() as client:
    # Inspect a function
    result = client.inspect_function("CL:CAR")
    print(result)
    
    # Evaluate Lisp code
    result = client.repl_eval("(+ 10 20)")
    print(result)  # {"result": "30"}
```

Install the client:
```bash
pip install cl-tron-mcp  # Coming soon
# Or use directly:
python cl_tron_client.py
```

### Opencode Integration

1. **Configure OpenCode** (`~/.config/opencode/opencode.json`):
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

2. **Start the MCP server**:
   ```bash
   # Using the start script (stdio transport)
   ./start-mcp.sh

   # Or directly
   sbcl --non-interactive \
     --eval '(ql:quickload :cl-tron-mcp :silent t)' \
     --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
   ```

### Cursor Integration

1. Install the MCP extension for Cursor
2. Copy `.cursor/mcp.json` to your Cursor settings or use the local workspace config:

```bash
# Copy to your Cursor settings
cp .cursor/mcp.json ~/.cursor/mcp.json
```

Or use the direct config (`.cursor/mcp-direct.json`):
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["sbcl", "--non-interactive", "--eval", "(ql:quickload :cl-tron-mcp :silent t)", "--eval", "(cl-tron-mcp/core:start-server :transport :stdio)"],
      "disabled": false,
      "env": {}
    }
  }
}
```

### VS Code Integration

1. Install the MCP extension for VS Code
2. Copy `.vscode/mcp.json` to your VS Code settings or use the local workspace config:

```bash
# Copy to your VS Code settings
cp .vscode/mcp.json ~/.vscode/mcp.json
```

Or configure directly in `.vscode/mcp.json`:
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["bash", "-c", "cd /home/emmanuel/quicklisp/local-projects/cl-tron-mcp && sbcl --non-interactive --eval '(ql:quickload :cl-tron-mcp :silent t)' --eval '(cl-tron-mcp/core:start-server :transport :stdio)'"],
      "disabled": false,
      "env": {}
    }
  }
}
```

### Claude Code CLI

```bash
# Start MCP server
sbcl --non-interactive \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio)'

# Claude Code can connect via MCP protocol
```

## Usage

### Quick Start

#### Lisp REPL

```lisp
;; Load the system
(ql:quickload :cl-tron-mcp)

;; Run the tutorial
(load "tutorial-run.lisp")

;; Or start the server
(cl-tron-mcp/core:start-server :transport :stdio)
```

#### Python Client

```python
# Demo
python cl_tron_client.py

# Or import in your code
from cl_tron_client import CLTronClient

with CLTronClient() as client:
    # List all available tools
    print(client.tools.keys())
    
    # Inspect a function
    result = client.inspect_function("CL:CAR")
    
    # Evaluate Lisp
    result = client.repl_eval("(+ 1 2 3)")
```

### Starting the Server

#### Stdio Transport (Primary)

```lisp
;; Start MCP server with stdio transport
(cl-tron-mcp/core:start-server :transport :stdio)
```

**Stdio requirements (for MCP clients):** When the server is launched by an MCP client (Cursor, Kilocode, Opencode), stdout must contain only newline-delimited JSON-RPC messages. The server uses log4cl for all activity logging (to stderr) and does not write banners or logs to stdout. When starting SBCL via `start-mcp.sh` for stdio, the script uses `--noinform` to suppress the SBCL banner and redirects its own echo output to stderr so the client sees only JSON. Compilation and load progress are silenced (`*compile-verbose*` / `*load-verbose*` nil) so first-run build output does not appear on stdout. If the client still fails on first start, precompile once in a REPL: `(ql:quickload :cl-tron-mcp)` then restart the MCP server.

#### HTTP Transport

HTTP transport is implemented using `usocket` (no external dependencies needed):

```lisp
;; Start MCP server on port 8080
(cl-tron-mcp/core:start-server :transport :http :port 8080)
```

**Note:** HTTP transport has some issues with thread handling. Stdio transport is recommended for production use.

HTTP Endpoints:
- `GET /` - List available tools
- `POST /rpc` - Send MCP JSON-RPC message
- `GET /health` - Health check

Example:
```bash
curl http://127.0.0.1:8080/health
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' \
  http://127.0.0.1:8080/rpc
```

#### WebSocket Transport

```lisp
;; Start MCP server with WebSocket on port 8081
(cl-tron-mcp/core:start-server :transport :websocket :port 8081)
```

Note: WebSocket transport is a placeholder. Full implementation requires additional dependencies.

#### Swank Integration

Connect CL-TRON-MCP to a running SBCL instance with Swank loaded for full IDE-like debugging:

```lisp
;; 1. Start SBCL with Swank server (in one terminal)
(ql:quickload :swank)
(swank:create-server :port 4005)
;; => Swank started on port 4005

;; 2. Connect CL-TRON-MCP (in another terminal)
(ql:quickload :cl-tron-mcp)

;; Connect to Swank
(cl-tron-mcp/swank:swank-connect :port 4005)
;; => (:SUCCESS T :HOST "127.0.0.1" :PORT 4005)

;; Evaluate code via Swank
(cl-tron-mcp/swank:swank-eval :code "(+ 10 20)" :package "CL-USER")
;; => (:VALUE "30" ...)

;; Get backtrace on error
(cl-tron-mcp/swank:swank-backtrace)

;; Inspect objects
(cl-tron-mcp/swank:swank-inspect :expression "*package*")

;; Get documentation
(cl-tron-mcp/swank:swank-autodoc :symbol "mapcar")

;; Disconnect when done
(cl-tron-mcp/swank:swank-disconnect)
```

**Available Swank Tools (13 total):**

| Tool | Description |
|------|-------------|
| `swank_connect` | Connect to Swank server |
| `swank_disconnect` | Disconnect from Swank |
| `swank_status` | Check connection status |
| `swank_eval` | Evaluate Lisp code |
| `swank_compile` | Compile Lisp code |
| `swank_threads` | List all threads |
| `swank_abort` | Abort a thread |
| `swank_interrupt` | Interrupt evaluation |
| `swank_backtrace` | Get backtrace |
| `swank_inspect` | Inspect objects |
| `swank_describe` | Describe symbols |
| `swank_autodoc` | Get documentation |
| `swank_completions` | Symbol completion |

 This enables the same workflow as SLIME/SLY in Emacs, but via MCP for AI agents.

#### nrepl Integration (Sly, CIDER)

Connect CL-TRON-MCP to a running SBCL with nrepl loaded (Sly, CIDER, etc.) for full IDE-like debugging:

```lisp
;; 1. Start SBCL with nrepl (Sly example)
(ql:quickload :sly)
(sly:nrepl-start :port 7888)
;; => nrepl started on port 7888

;; 2. Connect CL-TRON-MCP (in another terminal)
(ql:quickload :cl-tron-mcp)

;; Connect to nrepl
(cl-tron-mcp/nrepl:nrepl-connect :port 7888)
;; => (:SUCCESS T :HOST "127.0.0.1" :PORT 7888 ...)

;; Evaluate code via nrepl
(cl-tron-mcp/nrepl:nrepl-eval :code "(+ 10 20)" :package "CL-USER")
;; => (:VALUE "30" ...)

;; Get backtrace on error
(cl-tron-mcp/nrepl:nrepl-backtrace)

;; Inspect objects
(cl-tron-mcp/nrepl:nrepl-inspect :expression "*package*")

;; Get documentation
(cl-tron-mcp/nrepl:nrepl-doc :symbol "mapcar")

;; Disconnect when done
(cl-tron-mcp/nrepl:nrepl-disconnect)
```

**Available nrepl Tools (14 total):**

| Tool | Description |
|------|-------------|
| `nrepl_connect` | Connect to nrepl server |
| `nrepl_disconnect` | Disconnect from nrepl |
| `nrepl_status` | Check connection status |
| `nrepl_eval` | Evaluate Lisp code |
| `nrepl_compile` | Compile Lisp code |
| `nrepl_sessions` | List nrepl sessions |
| `nrepl_close_session` | Close a session |
| `nrepl_threads` | List all threads |
| `nrepl_interrupt` | Interrupt evaluation |
| `nrepl_backtrace` | Get backtrace |
| `nrepl_inspect` | Inspect objects |
| `nrepl_describe` | Describe symbols |
| `nrepl_doc` | Get documentation |
| `nrepl_completions` | Symbol completion |

 This enables the same workflow as CIDER/SLY in Emacs, but via MCP for AI agents.

#### Unified REPL Interface (Recommended)

CL-TRON-MCP provides a **unified REPL interface** that automatically detects and works with both Swank and nrepl:

```lisp
;; Auto-detect (tries Swank first, then nrepl)
(cl-tron-mcp/unified:repl-connect :port 4005)
;; => (:SUCCESS T :TYPE :SWANK ...)

;; Or explicitly specify
(cl-tron-mcp/unified:repl-connect :type :nrepl :port 7888)

;; Same API regardless of protocol!
(cl-tron-mcp/unified:repl-eval :code "(+ 10 20)")
(cl-tron-mcp/unified:repl-completions :prefix "mak")
(cl-tron-mcp/unified:repl-threads)
```

**Benefits:**
- No need to know which REPL you're using
- Auto-detection tries Swank (port 4005) first, then nrepl (port 7888)
- Same tool names work with any REPL
- Future-proof: works with any Swank or nrepl compatible tool

**Available Unified Tools (12):**

| Tool | Description |
|------|-------------|
| `repl_connect` | Connect to any REPL (auto-detect) |
| `repl_disconnect` | Disconnect from REPL |
| `repl_status` | Check connection status |
| `repl_eval` | Evaluate Lisp code |
| `repl_compile` | Compile Lisp code |
| `repl_threads` | List all threads |
| `repl_abort` | Abort/interrupt evaluation |
| `repl_backtrace` | Get backtrace |
| `repl_inspect` | Inspect object |
| `repl_describe` | Describe symbol |
| `repl_completions` | Get completions |
| `repl_doc` | Get documentation |

**MCP Tool Usage:**

```json
{
  "tool": "repl_connect",
  "arguments": {"port": 4005}
}

{
  "tool": "repl_eval",
  "arguments": {"code": "(+ 1 2 3)"}
}

{
  "tool": "repl_completions",
  "arguments": {"prefix": "mak"}
}
```

**Swank vs nrepl:**

| Feature | Swank | nrepl |
|---------|-------|-------|
| Slime | ✓ Native | Via compat |
| Portacle | ✓ Native | Via compat |
| Sly | ✗ | ✓ Native |
| CIDER | ✗ | ✓ Native |
| Protocol | S-expressions | JSON |

**Choose your REPL:**
- **Slime/Portacle**: Use `swank_connect` (port 4005)
- **Sly/CIDER**: Use `nrepl_connect` (port 7888)

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
├── cl_tron_client.py      # Python MCP client
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

CL-TRON-MCP uses SBCL's built-in `sb-introspect` library for cross-referencing:

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

Note: Dynamically created packages (e.g., created during compilation) may not be indexed by sb-introspect.

## Tutorial

Run the factorial tutorial to verify all tools are working:

```bash
sbcl --load tutorial-run.lisp
```

The tutorial demonstrates 10 key features:
1. **INSPECT-FUNCTION** - Inspect function definitions
2. **TRACE FUNCTION** - Trace function calls
3. **WHO-CALLS** - Find function callers
4. **DEBUGGER FRAMES** - Access stack frames
5. **REPL EVAL** - Evaluate Lisp code
6. **LOGGING** - Package-level logging
7. **SYSTEM INFO** - Runtime information
8. **RUNTIME STATS** - Memory and thread statistics
9. **HEALTH CHECK** - Server health verification
10. **APPROVAL CHECK** - Security verification

See `tutorial/` directory for additional debugging tutorials:
- `tutorial/README.md` - Tutorial guide
- `tutorial/debugging-tutorial.lisp` - Lisp code examples
- `tutorial/tutorial.json` - JSON format scenarios
- `tutorial/swank-tutorial.lisp` - Swank integration tutorial

## Troubleshooting

### MCP Client Issues

| Symptom | Cause | Solution |
| ------- | ----- | -------- |
| Client shows "failed" after startup | Server using wrong JSON key case | Ensure responses use lowercase keys (`jsonrpc`, `id`, `result`) |
| Client shows "failed" or parse error (stdio) | Non-JSON on stdout (banner, logs) | Use `start-mcp.sh` (uses `--noinform`, banner to stderr); do not write to stdout except JSON responses |
| No response to requests | Thread crash or buffering | Use stdio transport; ensure `force-output` is called |
| "Package not found" error | Quicklisp not loaded | Add `(ql:quickload :cl-tron-mcp)` before starting server |

### Lisp Runtime Issues

| Symptom | Cause | Solution |
| ------- | ----- | -------- |
| "Symbol not found" | Package not loaded | `(ql:quickload :cl-tron-mcp)` |
| "Approval timeout" | User not responding | Increase timeout or proceed without approval |
| "Transport bind failed" | Port in use | Use different port or kill conflicting process |
| Tests failing | Stale FASL files | `(asdf:compile-system :cl-tron-mcp :force t)` |
| "sb-introspect unavailable" | SBCL without sb-introspect | Most SBCL builds include it; reinstall if needed |
| Debugger features unavailable | SBCL compiled without :sb-dbg | Rebuild SBCL with debugging or use default fallbacks |
| "not a function" error | Case sensitivity in symbol | Use uppercase: `CL:CAR` not `cl:car` |

### Verifying Server Works

Test the server manually (use `--noinform` so stdout contains only JSON when piped):
```bash
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
```
Or use the script: `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh`

Expected response:
```json
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05",...}}
```

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
