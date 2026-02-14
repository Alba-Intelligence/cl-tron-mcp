# CL-TRON-MCP Documentation

## Overview

- **[Architecture](architecture.md)** — Recommended setup: one long-running Lisp session (Swank or nrepl) that you start; the MCP connects to it and interacts with all Swank facilities (eval, debugger, backtrace, stepping, inspect) the same way a user in Slime would.
- **[Swank Integration](swank-integration.md)** — Swank protocol implementation: wire protocol, client architecture, RPC operations reference.

## Tool Documentation

| Document | Description |
|----------|-------------|
| [tools/debugger.md](tools/debugger.md) | Debugger tools: frames, restarts, breakpoints, stepping |
| [tools/inspector.md](tools/inspector.md) | Inspector tools: objects, slots, classes, functions, packages |
| [tools/hot-reload.md](tools/hot-reload.md) | Hot reload: compile string, reload system |
| [tools/profiler.md](tools/profiler.md) | Profiler tools: start, stop, report |
| [tools/threads.md](tools/threads.md) | Thread tools: list, inspect, backtrace |
| [tools/monitor.md](tools/monitor.md) | Monitor tools: health, runtime stats, GC, system info |

## Quick Start

1. Start a Swank server in your Lisp:
   ```lisp
   (ql:quickload :swank)
   (swank:create-server :port 4005)
   ```

2. Connect the MCP:
   ```json
   {"name": "repl_connect", "arguments": {"port": 4005}}
   ```

3. Use the tools:
   ```json
   {"name": "repl_eval", "arguments": {"code": "(+ 1 2)"}}
   {"name": "repl_backtrace"}
   {"name": "repl_frame_locals", "arguments": {"frame": 0}}
   ```

See also AGENTS.md and README.md in the project root for setup, CRITICAL rules (stdio, log4cl, --noinform), and Swank/nrepl integration.
