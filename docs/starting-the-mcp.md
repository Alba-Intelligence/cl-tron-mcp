# Starting the MCP Server

This document explains how the MCP server is started and how to fix common "MCP won't start" issues.

## Who Starts the MCP

- **The MCP client** (Cursor, OpenCode, Kilocode) starts the server when you add it to your config and (re)start the client. The client runs the configured command (e.g. `start-mcp.sh`).
- **The AI agent** only uses tools *after* the server is running. The agent does not start the server; if the server fails to start, the cause is client config or environment.

## Portable Config Pattern

Copy the example that matches your client from **`examples/`** (e.g. `examples/cursor-mcp.json.example`) and **replace `/path/to/cl-tron-mcp`** with the full path to your `cl-tron-mcp` directory (e.g. `~/quicklisp/local-projects/cl-tron-mcp`). Do not commit machine-specific paths; use the examples as templates.

Example (Cursor):

```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": ["bash", "-c", "cd /path/to/cl-tron-mcp && ./start-mcp.sh"],
      "disabled": false,
      "env": {}
    }
  }
}
```

Replace `/path/to/cl-tron-mcp` with your path.

2. **Use `start-mcp.sh`** so stdout stays clean (no SBCL banner; script output goes to stderr). If you run SBCL directly, always use **`--noinform`** so the first line the client sees is JSON.

## One-Time Precompile (Avoid First-Start Timeout)

The first time the client starts the server, SBCL may compile the system, which can take 1–2 minutes. Many clients use a startup timeout (e.g. 30–60 seconds) and may report "failed to start" if the first JSON line does not appear in time.

**Do this once** before relying on the client to start the MCP:

```bash
cd /path/to/cl-tron-mcp
sbcl --noinform --eval '(ql:quickload :cl-tron-mcp)' --eval '(quit)'
```

Replace `/path/to/cl-tron-mcp` with your path. After this, the next client start should be fast.

## Troubleshooting Checklist

If the MCP or Tron "doesn't start" or the client says the server failed:

1. **Replace path in config** — Ensure the command uses the correct path to `cl-tron-mcp` (and to `start-mcp.sh`). Use the same path you used for the one-time precompile.
2. **Precompile once** — Run the one-time precompile command above so the first client start stays under the client's timeout.
3. **SBCL in PATH** — The client runs the command in its own environment. Ensure `sbcl` is on the PATH when the client starts the server (e.g. if you use a login shell, the client might not source your profile).
4. **No stdout pollution** — Use `start-mcp.sh` (recommended) or, if you run SBCL directly, include `--noinform`. Any output on stdout before the first JSON line will break the MCP handshake.

## See Also

- [AGENTS.md](../AGENTS.md) — Quick start for agents
- [README: Quick Start](../README.md#quick-start) — Client config snippets
- [examples/](../examples/) — Portable example configs (copy and set your path)
