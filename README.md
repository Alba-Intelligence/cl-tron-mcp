# CL-TRON-MCP

**AI-Powered Debugging for Common Lisp**

A Model Context Protocol (MCP) server that gives AI assistants deep access to running SBCL Common Lisp applications—debugger, inspector, profiler, and hot code reload.

## How It Works

### See It In Action

**Basic Debugging Demo:**

![Basic Demo](demo/demo.gif)

The AI connects to your running Lisp, triggers an error, inspects the backtrace, hot-reloads a fix, and verifies it works—all without restarting.

---

**Kilocode CLI Demo:**

![Kilocode Demo](demo/demo-kilocode.gif)

Shows what you'd see when using Kilocode with Tron MCP: the terminal interface, commands, and output.

---

**Raw MCP Protocol:**

![MCP Protocol](demo/demo-mcp-raw.gif)

Shows the actual JSON-RPC messages that AI clients send to Tron internally. This is what Kilocode, Cursor, and Claude Code do under the hood.

---

### Architecture

```
┌─────────────────┐         ┌─────────────────┐         ┌─────────────────┐
│  SBCL + Swank   │◄───────►│   Tron (MCP)    │◄───────►│   AI Client     │
│  (Port 4005)    │         │   (stdio)       │         │ (Kilocode, etc) │
│                 │         │                 │         │                 │
│  Your code      │         │   99 tools:     │         │  Sends prompts  │
│  Debugger       │         │   - swank_eval  │         │  Receives       │
│  Threads        │         │   - inspect     │         │  results        │
│  State lives    │         │   - profile     │         │                 │
└─────────────────┘         └─────────────────┘         └─────────────────┘
        ▲
        │
        └──────────────────────────────────────────────────────┘
                      Same session, no restart
```

**Key insight:** All state lives in the SBCL process. Tron connects as a client. The session persists across debugging, hot-reloads, and errors.

📖 **[Full architecture documentation →](docs/architecture.md)**

## Features

| Category | Description | Documentation |
|----------|-------------|---------------|
| **Debugger** | Backtrace, restarts, stepping, breakpoints | [docs/tools/debugger.md](docs/tools/debugger.md) |
| **Inspector** | Objects, slots, classes, functions, packages | [docs/tools/inspector.md](docs/tools/inspector.md) |
| **Hot Reload** | Compile strings, reload systems | [docs/tools/hot-reload.md](docs/tools/hot-reload.md) |
| **Profiler** | Start/stop profiling, generate reports | [docs/tools/profiler.md](docs/tools/profiler.md) |
| **Threads** | List, inspect, get backtraces | [docs/tools/threads.md](docs/tools/threads.md) |
| **Monitor** | Health checks, runtime stats, GC | [docs/tools/monitor.md](docs/tools/monitor.md) |
| **Swank** | Slime/Portacle integration (21 tools) | [docs/swank-integration.md](docs/swank-integration.md) |

**99 tools total** across 14 categories.

### Quick Tool Examples

**Debug an error:**
```lisp
(swank-eval :code "(my-buggy-function 7)")  ; triggers error
(swank-backtrace)                            ; see stack frames
(swank-invoke-restart :restart_index 2)      ; abort
(swank-eval :code "(defun my-buggy-function ...)")  ; hot-reload fix
```

**Profile performance:**
```lisp
(profile-start)
(swank-eval :code "(process-data)")
(profile-stop)
(profile-report :format "flat")
```

**Find callers:**
```lisp
(who-calls :symbol_name "my-package:process")
```

📖 **[More workflow examples →](prompts/workflow-examples.md)**

## Quick Start

### 1. Start a Swank Server

```lisp
;; In SBCL
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
```

### 2. Configure Your AI Client

**Kilocode** (`~/.kilocode/cli/config.json`):
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["/path/to/cl-tron-mcp/start-mcp.sh"]
    }
  }
}
```

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

Ask your AI: *"Connect to Swank on port 4005 and debug factorial-example.lisp"*

## Installation

### Quicklisp

```lisp
(ql:quickload :cl-tron-mcp)
```

### From Source

```bash
git clone https://github.com/yourusername/cl-tron-mcp.git
cd cl-tron-mcp
sbcl --eval '(load "cl-tron-mcp.asd")' --eval '(ql:quickload :cl-tron-mcp)'
```

## Development

### Running Tests

```lisp
(asdf:test-system :cl-tron-mcp)

;; Or with Rove
(ql:quickload :rove)
(rove:run :cl-tron-mcp/tests)
```

### Project Structure

```
cl-tron-mcp/
├── src/                    # Source code
│   ├── core/               # Core infrastructure
│   ├── swank/              # Swank client
│   ├── nrepl/              # nrepl client
│   ├── tools/              # Tool definitions
│   └── ...
├── tests/                  # Rove test suites
├── docs/                   # Documentation
│   ├── architecture.md     # How it works
│   ├── swank-integration.md
│   └── tools/              # Tool docs
├── prompts/                # Workflow guides
├── demo/                   # Demo generation
└── AGENTS.md              # AI agent guidelines
```

### Documentation

| Document | Purpose |
|----------|---------|
| [AGENTS.md](AGENTS.md) | Quick start for AI agents using Tron |
| [docs/architecture.md](docs/architecture.md) | System architecture and design |
| [docs/swank-integration.md](docs/swank-integration.md) | Swank protocol details |
| [docs/demo-creation.md](docs/demo-creation.md) | How to create demo GIFs |
| [prompts/workflow-examples.md](prompts/workflow-examples.md) | Step-by-step usage examples |
| [prompts/debugging-workflows.md](prompts/debugging-workflows.md) | Debugging patterns |

### Contributing

1. Fork the repository
2. Create a feature branch
3. Follow guidelines in [AGENTS.md](AGENTS.md)
4. Add tests
5. Submit a pull request

## Requirements

- **SBCL** 2.0.0 or later
- **Quicklisp**
- **Swank** (for Slime) or **cl-nrepl** (for nrepl)

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Package not found" | `(ql:quickload :cl-tron-mcp)` first |
| Client shows "failed" | Use `start-mcp.sh` which handles stdio correctly |
| "Not connected to REPL" | Run `swank_connect` or `repl_connect` first |
| Tests fail with stale FASL | `(asdf:compile-system :cl-tron-mcp :force t)` |

## License

MIT-style. See LICENSE file.

## Resources

- [SBCL Documentation](http://www.sbcl.org/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [Swank/Slime](https://slime.common-lisp.dev/)