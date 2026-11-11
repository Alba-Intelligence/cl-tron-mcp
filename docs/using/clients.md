# Connecting an MCP client

Tron speaks MCP over **stdio** (the client launches it) or **HTTP** (long-running, port 4006).
Start with [../starting-the-mcp.md](../starting-the-mcp.md) for installation and transport choice;
this page only points to the client configs.

| Client | Config |
|---|---|
| Cursor | `.cursor/mcp.json` (copy to `~/.cursor/mcp.json`); example: `examples/cursor-mcp.json.example` |
| Kilocode | `.kilocode/mcp.json`; example: `examples/kilocode-mcp.json.example` |
| OpenCode | `examples/opencode-mcp.json.example` -> `~/.config/opencode/opencode.json` |
| Claude Code / others | Run `./start-mcp.sh --stdio-only`, or point an HTTP client at `http://127.0.0.1:4006/mcp` |

Notes:

- Use **one transport at a time** while debugging a client that will not connect.
- The Swank port (default 4005) is separate from the MCP HTTP port (default 4006).
- For stdio, nothing but JSON-RPC may reach stdout; logs go to stderr.
- Approvals need a confirmation code printed on the Tron terminal (stderr/TTY), which a human relays; keep that terminal away from the model. See [approval.md](approval.md).
- The `skill_store_installation_guide` tool returns per-client install notes.
- Client sample code: `examples/test_client.py`, `examples/cl_tron_client.py`.
