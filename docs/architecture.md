# Architecture

Tron's architecture is built around a simple constraint: **the live Lisp state stays in the target Swank session**. Tron provides MCP access to that session; it does not replace it.

## Process Model

### 1. Target Lisp session

This is the SBCL (or other Common Lisp) image that contains your application code.

- You start it.
- You load Swank into it.
- The debugger, stack frames, objects, threads, and loaded systems all live here.

Typical startup:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

### 2. Tron MCP server

This is a separate process started by the MCP client or by you through `start-mcp.sh`.

- It speaks MCP over stdio or HTTP.
- It exposes tools, resources, prompts, and approval flow.
- It connects to the target Lisp session as a Swank client.

### 3. MCP client / AI agent

The client launches or connects to Tron, then discovers and calls MCP tools such as `repl_connect`, `repl_eval`, `repl_backtrace`, `repl_compile`, and `repl_continue`.

## Runtime Flow

1. The target Lisp image starts Swank.
2. Tron starts and accepts MCP requests.
3. The agent calls `repl_connect` or `swank_connect`.
4. Tron forwards operations through Swank RPC.
5. Results come back through Tron as MCP tool responses.

That flow is what enables the "debug -> inspect -> patch -> continue" workflow without restarting the Lisp process.

## Main Source Boundaries

| Concern | Files |
| --- | --- |
| Server lifecycle | `src/core/server.lisp` |
| MCP protocol dispatch | `src/protocol/handlers*.lisp` |
| stdio / HTTP transport | `src/transport/stdio.lisp`, `src/transport/http-hunchentoot.lisp` |
| Tool registry | `src/tools/registry.lisp`, `src/tools/register-tools.lisp`, `src/tools/*.lisp` |
| Swank client | `src/swank/swank-connection.lisp`, `src/swank/swank-rpc.lisp`, `src/swank/swank-events.lisp`, `src/swank/swank-api.lisp` |
| Unified high-level REPL API | `src/unified/client.lisp` |
| Debugger wrappers | `src/debugger/` |
| Inspection | `src/inspector/` |
| Local hot-reload fallback | `src/hot-reload/core.lisp` |

## Transport Notes

- **stdio-only** is for MCP clients that launch Tron directly.
- **combined** and **http-only** are for longer-lived server setups.
- The transport choice changes how clients talk to Tron; it does **not** change where the Lisp state lives.

## Tool Layers

Tron exposes two overlapping REPL/debugger layers:

- **`repl_*` tools** - preferred high-level interface for agents
- **`swank_*` tools** - lower-level operations that map closely to raw Swank behavior

Other tool families (inspector, monitor, xref, logging, security, managed processes) sit alongside those layers.

## Caveats

- The richest workflow is still **SBCL + Swank**.
- Hot reload without a connected Swank session uses a local fallback path; advanced debugger interaction still requires a live Swank connection.
- Approval-sensitive tools are mediated by the security layer before execution.

## Related Docs

- [starting-the-mcp.md](starting-the-mcp.md)
- [code-reference.md](code-reference.md)
- [tools/index.md](tools/index.md)
- [../AGENTS.md](../AGENTS.md)
