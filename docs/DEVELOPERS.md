# Developer Guide: Improving Tron

This document is for contributors who want to add features, fix bugs, or refactor the Tron MCP server. For first-time setup, PR process, and code style, see [CONTRIBUTING.md](../CONTRIBUTING.md). For high-level design (two-process model, Swank client), see [architecture.md](architecture.md).

## Audience and purpose

- **You want to:** add a tool, add a transport, add an MCP resource or prompt, fix a bug, or understand where to change code.
- **Start here:** [CONTRIBUTING.md](../CONTRIBUTING.md) (setup, running tests) and this guide (where things live, how to add features).

## Where things live

| Area | Location | Notes |
|------|----------|--------|
| **Transport** | `src/transport/` | stdio, HTTP (Hunchentoot), WebSocket. Start/stop wired in `src/core/server.lisp`. |
| **Protocol** | `src/protocol/` | JSON-RPC 2.0 handling, message dispatch. |
| **Tools** | `src/tools/` | Registry, define-tool, and `src/<category>/` (debugger, inspector, swank, repl, profiler, etc.). |
| **MCP resources** | `src/resources/handler.lisp` | Whitelist and path resolution. On-disk docs under repo root (`AGENTS.md`, `docs/`, etc.). |
| **MCP prompts** | `src/prompts/handler.lisp` | Prompt registry. Optional markdown under `prompts/`. |
| **Server entry** | `src/core/server.lisp` | `start-server`, transport selection (`:combined`, `:stdio-only`, `:http-only`), `stop-server`. |
| **Scripts** | `scripts/` | `start-mcp.sh` (root), `run-http.sh`, `run-http-server.lisp`, `tutorial-run.lisp`, `debug-mcp-stdio.sh`. |
| **Examples** | `examples/` | MCP client config examples and example Python clients. |

## How to add a feature

### New tool

1. Create or extend a file in `src/<category>/` (e.g. `src/debugger/`, `src/inspector/`).
2. Register in `src/tools/register-tools.lisp`: `register-tool` and `register-tool-handler`.
3. Export the function from the package in `src/<category>/package.lisp`.
4. Add a test in `tests/` (e.g. `tests/<category>-test.lisp`).
5. Add the component to `cl-tron-mcp.asd` if you introduced a new file.

See [CONTRIBUTING.md – Adding New Tools](../CONTRIBUTING.md#adding-new-tools) for the full pattern and code style.

### New transport

1. Add the transport implementation under `src/transport/` (e.g. new file or extend existing).
2. In `src/core/server.lisp`: add a branch in the `case` in `start-server` and in `stop-server` for the new transport key.
3. If the transport blocks, support a non-blocking mode (e.g. `:block nil`) when used in combined mode; see HTTP transport.
4. Optionally add a `start-mcp.sh` branch and document in [starting-the-mcp.md](starting-the-mcp.md).

### New MCP resource (documentation file)

1. Add the file under the project root (e.g. `docs/my-feature.md`).
2. Add its path (relative to project root) to the whitelist in `src/resources/handler.lisp` (`initialize-default-whitelist` or `add-resource-to-whitelist`).
3. Resources are exposed via `resources/list` and `resources/read`; URIs are typically `file://path/from/root`.

### New prompt (guided workflow)

1. Register the prompt in `src/prompts/handler.lisp` (name, description, messages).
2. Optionally add markdown under `prompts/` and reference it from the prompt content or from docs.

## Testing and debugging

- **Run tests:** `(asdf:test-system :cl-tron-mcp)` or see [CONTRIBUTING.md – Running Tests](../CONTRIBUTING.md#running-tests).
- **Run MCP manually (stdio):** From repo root, `./start-mcp.sh --stdio-only` and pipe JSON (e.g. `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh --stdio-only 2>/dev/null | head -1`).
- **Kilocode-style debug:** `./scripts/debug-mcp-stdio.sh` (writes to `reports/mcp-stdio-out.log` and `reports/mcp-stdio-err.log`).
- **Transport options:** Combined (default), stdio-only, http-only. See [starting-the-mcp.md](starting-the-mcp.md).

## Conventions

- **Logging and stdout:** All server activity via log4cl; for stdio transport, only JSON-RPC goes to stdout. See [AGENTS.md – Stdout purity](../AGENTS.md) and [AGENTS.md – Logging](../AGENTS.md).
- **JSON keys:** MCP responses must use lowercase JSON keys (e.g. `:|jsonrpc|`, `:|result|`).
- **Naming and style:** [CONTRIBUTING.md – Code Style](../CONTRIBUTING.md#code-style).
- **Approval:** Tools that modify running code or execute code require approval; see [AGENTS.md – Security](../AGENTS.md).

## References

- [CONTRIBUTING.md](../CONTRIBUTING.md) — Setup, project structure, adding tools, tests, PR process.
- [architecture.md](architecture.md) — Two-process model, Swank client, debugger integration.
- [starting-the-mcp.md](starting-the-mcp.md) — Transport modes, Kilocode, troubleshooting.
- [docs/tools/](tools/) — Tool documentation (debugger, inspector, hot-reload, profiler, etc.).
- [MCP specification](https://modelcontextprotocol.io/).
