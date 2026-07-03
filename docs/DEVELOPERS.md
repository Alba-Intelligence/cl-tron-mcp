# Developer Guide

This guide is for maintainers who need to understand where behavior lives and where to make changes. For setup and test commands, start with [../CONTRIBUTING.md](../CONTRIBUTING.md). For a broader index of files, see [code-reference.md](code-reference.md).

## Mental Model

Tron has three layers:

1. **Transport and protocol** - accept MCP requests over stdio or HTTP
2. **Tool surface** - validate MCP inputs and expose the tool catalog
3. **Runtime integration** - connect to Swank and operate on the live Lisp session

The most important design constraint is that **the live Lisp state remains in the target Swank session**. Tron is a client of that session.

## Where to Change What

| Goal | Primary files |
| --- | --- |
| Add or change an MCP method | `src/protocol/handlers*.lisp` |
| Add or change a tool | `src/tools/*.lisp` plus the underlying subsystem in `src/<area>/` |
| Change server startup or transport selection | `src/core/server.lisp`, `src/transport/`, `start-mcp.sh` |
| Change Swank connection or debugger behavior | `src/swank/`, `src/unified/`, `src/debugger/` |
| Change inspection behavior | `src/inspector/`, `src/tools/inspector-tools.lisp` |
| Change hot reload behavior | `src/hot-reload/core.lisp`, `src/tools/hot-reload-tools.lisp` |
| Change approval flow | `src/security/`, `src/protocol/handlers-tools.lisp` |
| Change docs/resources/prompt discoverability | `src/resources/`, `src/prompts/`, `AGENTS.md`, `docs/` |

## Core Files

### Server and Transport

- [`src/core/server.lisp`](../src/core/server.lisp) - server lifecycle and transport orchestration
- [`src/transport/stdio.lisp`](../src/transport/stdio.lisp) - stdio MCP loop
- [`src/transport/http-hunchentoot.lisp`](../src/transport/http-hunchentoot.lisp) - HTTP transport implementation
- [`start-mcp.sh`](../start-mcp.sh) - runtime launcher used by clients and users

### Protocol Handlers

- [`src/protocol/handlers.lisp`](../src/protocol/handlers.lisp) - top-level dispatch
- [`src/protocol/handlers-tools.lisp`](../src/protocol/handlers-tools.lisp) - tool execution and approval flow
- [`src/protocol/handlers-resources.lisp`](../src/protocol/handlers-resources.lisp) - resources API
- [`src/protocol/handlers-prompts.lisp`](../src/protocol/handlers-prompts.lisp) - prompts API

### Swank / REPL Integration

- [`src/swank/swank-connection.lisp`](../src/swank/swank-connection.lisp)
- [`src/swank/swank-rpc.lisp`](../src/swank/swank-rpc.lisp)
- [`src/swank/swank-events.lisp`](../src/swank/swank-events.lisp)
- [`src/swank/swank-api.lisp`](../src/swank/swank-api.lisp)
- [`src/unified/client.lisp`](../src/unified/client.lisp)

### Tool Registration

- [`src/tools/macros.lisp`](../src/tools/macros.lisp) - tool-definition helpers
- [`src/tools/registry.lisp`](../src/tools/registry.lisp) - descriptors and registry access
- [`src/tools/register-tools.lisp`](../src/tools/register-tools.lisp) - bulk registration

## Tool Categories

The MCP surface is split across these files:

| File | Focus |
| --- | --- |
| `src/tools/inspector-tools.lisp` | object/class/function/package inspection |
| `src/tools/debugger-tools.lisp` | debugger views, breakpoints, frame stepping |
| `src/tools/repl-tools.lisp` | legacy direct REPL tool |
| `src/tools/hot-reload-tools.lisp` | compile/load and ASDF reload |
| `src/tools/profiler-tools.lisp` | profiling |
| `src/tools/tracer-tools.lisp` | function tracing |
| `src/tools/thread-tools.lisp` | thread introspection |
| `src/tools/monitor-tools.lisp` | health/runtime information |
| `src/tools/logging-tools.lisp` | log4cl control |
| `src/tools/xref-tools.lisp` | cross-reference queries |
| `src/tools/security-tools.lisp` | approval whitelist management |
| `src/tools/swank-tools.lisp` | raw Swank operations |
| `src/tools/process-tools.lisp` | managed SBCL+Swank child processes |
| `src/tools/unified-tools.lisp` | preferred high-level `repl_*` workflow |

## Typical Change Workflows

### Add a Tool

1. Implement or extend the behavior under `src/<area>/`.
2. Register the tool in the matching `src/tools/<category>-tools.lisp`.
3. Export any new symbols from the subsystem package file.
4. Add tests in `tests/`.
5. Add a tool page under `docs/tools/`.

### Change Hot Reload or Debugger Semantics

1. Add a failing regression test in `tests/hot-reload-test.lisp`, `tests/swank-integration-test.lisp`, or `tests/mcp-e2e-test.lisp`.
2. Trace the underlying behavior through `src/unified/`, `src/swank/`, or `src/hot-reload/`.
3. Update the tool wrapper if the MCP-facing arguments or return shape changed.
4. Update the relevant docs in `docs/tools/`, `README.md`, or `docs/starting-the-mcp.md`.

### Change Startup / Packaging Behavior

1. Update `start-mcp.sh` first if the change affects how users or clients launch Tron.
2. Adjust `run-mcp.sh` only if the optional `devenv` wrapper also needs to change.
3. Keep the Quicklisp-first install path working and documented.

## Testing Map

| Test file | What it covers |
| --- | --- |
| `tests/protocol-test.lisp` | protocol handlers and request validation |
| `tests/mcp-e2e-test.lisp` | end-to-end MCP request behavior |
| `tests/swank-test.lisp` | Swank client helpers |
| `tests/swank-integration-test.lisp` | live Swank session integration |
| `tests/hot-reload-test.lisp` | local compile/load and reload helpers |
| `tests/process-manager-test.lisp` | managed Swank child-process lifecycle |

## Documentation Responsibilities

When you change behavior, update the docs closest to that behavior:

- startup/install changes -> `README.md`, `docs/starting-the-mcp.md`
- new or changed tool -> `docs/tools/`
- new subsystem or source map changes -> `docs/code-reference.md`
- contributor workflow changes -> `CONTRIBUTING.md`
- agent discoverability changes -> `AGENTS.md`, `docs/agents/`
