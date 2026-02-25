# Architecture: One Long-Running Lisp Session

This document describes the recommended setup so the MCP can interact with a Lisp session (Swank) the same way a user in Slime would: see output, debugger state, step, move frames, invoke restarts, inspect, and compile.

## Two Processes

### 1. Lisp session (Swank)

- **Who starts it:** The user (or automation). You start one SBCL (or other Lisp) with Swank and **leave it running**.
- **Role:** This is the single session where all code is loaded and executed—by you or by the MCP. The debugger runs here. Slime/Sly/Emacs can attach to the same session.
- **Typical start:** `(ql:quickload :swank)` then `(swank:create-server :port 4006)` (default port 4006).
- **Dedicated port for MCP:** Use a separate Swank port for MCP (e.g. Swank on 4006) so you keep 4006 for your editor: `(swank:create-server :port 4006 :dont-close t)`.

### 2. MCP server

- **Who starts it:** The MCP client (Cursor, Kilocode, Opencode) via `start-mcp.sh` or the configured command. It runs in a **separate process**.
- **Role:** Handles JSON-RPC over stdio (or HTTP via Hunchentoot / WebSocket). Connects to the Lisp session as a **Swank client**—the same way Slime does. The MCP then uses Swank facilities (eval, backtrace, restarts, stepping, inspect, compile) over the protocol.
- **Connection:** After the MCP server is running, it must connect to your Lisp session (e.g. via MCP tool `repl_connect` or `swank_connect` with the port you used). Once connected, tools like `repl_eval`, `repl_backtrace`, `repl_inspect` operate on that session.

## Agent Workflow

1. **You:** Start the Lisp session with Swank (port 4006). Keep it running.
2. **Client:** Starts the MCP server (e.g. Cursor runs `start-mcp.sh`). The agent (or you) connects the MCP to your session via `repl_connect` or `swank_connect`.
3. **Agent:** Uses MCP tools (`repl_eval`, `repl_backtrace`, `repl_inspect`, `repl_compile`, etc.) to load code, run it, see output and debugger state, step, move frames, invoke restarts, and fix code—all through the connected session. No second REPL; one session, MCP as a client of it.

## Without a Connected Session

If the MCP is not connected to any Swank session, tools that require it (e.g. `repl_eval`) return an error such as "Not connected to any REPL". Other tools (e.g. `inspect_function`, `health_check`, `tools/list`) do not require a REPL and work as long as the MCP server is running.

## Implementation

### Swank Client

The MCP implements a full Swank protocol client:

- **Protocol Layer** (`src/swank/protocol.lisp`): Length-prefixed message framing, UTF-8 encoding
- **Client Layer** (`src/swank/client.lisp`): Connection management, reader thread, request/response correlation, event queue

See [swank-integration.md](swank-integration.md) for detailed protocol and API documentation.

### Debugger Integration

The debugger tools (`src/debugger/`) are thin wrappers over Swank RPC:

- `debugger_frames` → `swank:backtrace`
- `debugger_restarts` → `swank:compute-restarts-for-emacs`
- `step_frame` → `swank:sldb-step-into/next/out`
- `breakpoint_set` → `swank:break`

See [tools/debugger.md](tools/debugger.md) for tool usage.

## See Also

- [README: Swank Integration](../README.md#swank-integration-recommended-for-agent-workflow) — step-by-step setup.
- [AGENTS.md: Recommended Workflow](../AGENTS.md#recommended-workflow-one-long-running-lisp-session) — agent-oriented summary.
- [swank-integration.md](swank-integration.md) — Swank protocol implementation details.
- [docs/tools/](tools/) — tool documentation (debugger, inspector, hot-reload, etc.).
