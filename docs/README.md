# Documentation map

Two audiences, two entry points.

## Using Tron (agents and the people who run them)

Served to MCP clients via `resources/read`. Start with the quickstart.

| Document | Purpose |
|---|---|
| [using/quickstart.md](using/quickstart.md) | Connect, the tool loop, approval, debugging. **Read first.** |
| [using/tools.md](using/tools.md) | Every tool and its approval level (generated) |
| [using/approval.md](using/approval.md) | The approval exchange and its trust boundary |
| [using/errors.md](using/errors.md) | Error-code table (generated); details in [errors/](errors/README.md) |
| [using/troubleshooting.md](using/troubleshooting.md) | Connection, startup and Swank problems |
| [using/clients.md](using/clients.md) | Client configuration pointers |
| [tools/](tools/) | One page per tool: parameters, returns, examples |
| [starting-the-mcp.md](starting-the-mcp.md) | Install, start, transports, verification |
| [swank-integration.md](swank-integration.md) | Swank specifics |

## Changing Tron (contributors and their agents)

Start with [../AGENTS.md](../AGENTS.md).

| Document | Purpose |
|---|---|
| [developing/workflow.md](developing/workflow.md) | Environment, branches, tests, docs, reports, commits |
| [developing/conventions.md](developing/conventions.md) | Coding style and naming |
| [developing/adding-a-tool.md](developing/adding-a-tool.md) | Register a tool, test it, document it |
| [developing/transports.md](developing/transports.md) | stdio/HTTP/websocket, HTTP security, logging |
| [architecture.md](architecture.md) | Process model and source boundaries |
| [code-reference.md](code-reference.md) | Source-level map: packages, scripts, tests |
| [DEVELOPERS.md](DEVELOPERS.md) | Where to change what |
| [protocol-handlers.md](protocol-handlers.md) | JSON-RPC/MCP handler structure |
| [mcp-resources-prompts.md](mcp-resources-prompts.md) | MCP resources and prompts implementation |
| [demo-creation.md](demo-creation.md) | Regenerating the GIF demos |

## Plans and reports

| Path | Purpose |
|---|---|
| [plans/](plans/) | Roadmap and design plans (planned tools live here, not in user docs) |
| [../reports/README.md](../reports/README.md) | Dated reviews and diagnostics, with status |

## Rules for documentation

One source of truth per fact; generated where enumerable; checked by `tests/docs-test.lisp`. Details in
[developing/workflow.md](developing/workflow.md#4-documentation-rules).
