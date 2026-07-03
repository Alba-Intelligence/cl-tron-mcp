# Code Reference

This document is the source-level map of `cl-tron-mcp`: what each subsystem does, where the main entry points live, and which files you usually need to read or modify for a given kind of change.

## System at a Glance

Tron is an MCP server that sits between an MCP client and a live Lisp session:

1. **The MCP client** starts Tron through `start-mcp.sh`.
2. **Tron** serves MCP over stdio or HTTP and exposes tools, resources, prompts, and approval flow.
3. **Tron connects to Swank** and uses that connection to evaluate code, inspect objects, read debugger state, invoke restarts, and hot-reload definitions inside the target Lisp session.

The canonical ASDF definitions live in [`cl-tron-mcp.asd`](../cl-tron-mcp.asd).

## Repository Layout

| Path | Purpose |
| --- | --- |
| `src/core/` | Server lifecycle, configuration, metrics, errors, request tracing |
| `src/protocol/` | JSON-RPC request parsing and MCP method handlers |
| `src/transport/` | stdio, HTTP/Hunchentoot, and WebSocket transport entry points |
| `src/tools/` | Tool registry plus MCP-facing tool definitions |
| `src/swank/` | Swank client protocol, RPC, event handling, process management |
| `src/unified/` | Unified `repl_*` abstraction layered on top of Swank |
| `src/debugger/` | Debugger-oriented helpers such as frames, restarts, and breakpoints |
| `src/inspector/` | Object, package, class, and function inspection |
| `src/hot-reload/` | Local fallback compilation/reload helpers |
| `src/profiler/` | Profiling start/stop/report behavior |
| `src/tracer/` | Function tracing |
| `src/monitor/` | Health and runtime statistics |
| `src/logging/` | log4cl integration and stdout/stderr safety |
| `src/xref/` | Cross-reference utilities such as `who_calls` |
| `src/security/` | Approval requests, whitelist handling, audit logging |
| `src/resources/` | MCP `resources/list` and `resources/read` support |
| `src/prompts/` | MCP `prompts/list` and `prompts/get` support |
| `tests/` | Rove unit/integration coverage for protocol, tools, startup, and Swank |
| `docs/` | User docs, developer docs, tool reference, architecture docs |
| `examples/` | MCP client configuration examples and example clients |
| `scripts/` | Helper scripts for demos and MCP debugging |

## Runtime Entry Points

### Starting and Stopping the MCP Server

- [`src/core/server.lisp`](../src/core/server.lisp)
  - `start-server`
  - `stop-server`
  - `get-server-state`
  - `get-transport-type`

### Protocol Dispatch

- [`src/protocol/handlers.lisp`](../src/protocol/handlers.lisp) - top-level request routing
- [`src/protocol/handlers-initialize.lisp`](../src/protocol/handlers-initialize.lisp) - `initialize`
- [`src/protocol/handlers-tools.lisp`](../src/protocol/handlers-tools.lisp) - `tools/list`, `tools/call`, `approval/respond`
- [`src/protocol/handlers-resources.lisp`](../src/protocol/handlers-resources.lisp) - `resources/list`, `resources/read`
- [`src/protocol/handlers-prompts.lisp`](../src/protocol/handlers-prompts.lisp) - `prompts/list`, `prompts/get`
- [`src/protocol/handlers-ping.lisp`](../src/protocol/handlers-ping.lisp) - ping/health-style method

### Tool Registration

- [`src/tools/registry.lisp`](../src/tools/registry.lisp) - in-memory registry and descriptor lookup
- [`src/tools/macros.lisp`](../src/tools/macros.lisp) - `define-validated-tool`, `define-simple-tool`
- [`src/tools/register-tools.lisp`](../src/tools/register-tools.lisp) - bulk registration

## Tool Surface

The current registry contains **91 tools**.

| Category | File | Count | Notes |
| --- | --- | ---: | --- |
| Inspector | `src/tools/inspector-tools.lisp` | 5 | Runtime object and symbol inspection |
| Debugger | `src/tools/debugger-tools.lisp` | 7 | Frames, restarts, breakpoints, frame stepping |
| REPL | `src/tools/repl-tools.lisp` | 1 | Legacy direct REPL entrypoint |
| Hot reload | `src/tools/hot-reload-tools.lisp` | 2 | Local compile/load fallback and ASDF reload |
| Profiler | `src/tools/profiler-tools.lisp` | 3 | Start, stop, report |
| Tracer | `src/tools/tracer-tools.lisp` | 3 | Add/remove/list traces |
| Threads | `src/tools/thread-tools.lisp` | 3 | Thread inspection and backtrace |
| Monitor | `src/tools/monitor-tools.lisp` | 4 | Health, runtime stats, GC, system info |
| Logging | `src/tools/logging-tools.lisp` | 5 | log4cl control and message emission |
| XRef | `src/tools/xref-tools.lisp` | 5 | `who_*` and callee inspection |
| Security | `src/tools/security-tools.lisp` | 5 | Approval whitelist management |
| Swank | `src/tools/swank-tools.lisp` | 21 | Raw Swank-oriented workflow |
| Managed processes | `src/tools/process-tools.lisp` | 4 | Launch/list/status/kill SBCL+Swank children |
| Unified REPL | `src/tools/unified-tools.lisp` | 24 | Preferred `repl_*` workflow for agents |

Use [docs/tools/index.md](tools/index.md) for the human-readable catalog of every tool page.

## Subsystem Guide

### Swank Integration

Read these first if the change touches connection state, debugger behavior, evaluation, or REPL tools:

- [`src/swank/swank-connection.lisp`](../src/swank/swank-connection.lisp)
- [`src/swank/swank-rpc.lisp`](../src/swank/swank-rpc.lisp)
- [`src/swank/swank-events.lisp`](../src/swank/swank-events.lisp)
- [`src/swank/swank-api.lisp`](../src/swank/swank-api.lisp)
- [`src/unified/client.lisp`](../src/unified/client.lisp)

### Debugger Flow

If you need stack frames, restarts, breakpoints, or stepping:

- [`src/debugger/frames.lisp`](../src/debugger/frames.lisp)
- [`src/debugger/restarts.lisp`](../src/debugger/restarts.lisp)
- [`src/debugger/breakpoints.lisp`](../src/debugger/breakpoints.lisp)
- [`src/debugger/stepping.lisp`](../src/debugger/stepping.lisp)

### Hot Reload

- [`src/hot-reload/core.lisp`](../src/hot-reload/core.lisp) - local fallback compile/load and ASDF reload
- [`src/tools/hot-reload-tools.lisp`](../src/tools/hot-reload-tools.lisp) - MCP-facing wrappers

### Approval and Security

- [`src/security/approval.lisp`](../src/security/approval.lisp)
- [`src/security/audit.lisp`](../src/security/audit.lisp)
- [`src/tools/security-tools.lisp`](../src/tools/security-tools.lisp)

### HTTP / stdio Transport

- [`src/transport/stdio.lisp`](../src/transport/stdio.lisp)
- [`src/transport/http.lisp`](../src/transport/http.lisp)
- [`src/transport/http-hunchentoot.lisp`](../src/transport/http-hunchentoot.lisp)
- [`start-mcp.sh`](../start-mcp.sh)

## Scripts and Operational Files

| File | Purpose |
| --- | --- |
| [`start-mcp.sh`](../start-mcp.sh) | Primary entrypoint for users and MCP clients |
| [`run-mcp.sh`](../run-mcp.sh) | Optional wrapper that enters `devenv` before starting Tron |
| [`create_configs.sh`](../create_configs.sh) | Generate MCP client config files |
| [`scripts/debug-mcp-stdio.sh`](../scripts/debug-mcp-stdio.sh) | Capture stdio startup output for debugging |
| [`scripts/run-http-server.lisp`](../scripts/run-http-server.lisp) | Helper for HTTP-centric workflows |
| [`devenv.nix`](../devenv.nix) | Optional Nix/devenv development shell |

## Test Layout

| File | Focus |
| --- | --- |
| `tests/core-test.lisp` | core helpers and version/config behavior |
| `tests/protocol-test.lisp` | MCP protocol handler behavior |
| `tests/security-test.lisp` | approval flow and whitelist logic |
| `tests/transport-test.lisp` | transport startup/response behavior |
| `tests/swank-test.lisp` | Swank client behavior without full live workflow |
| `tests/swank-integration-test.lisp` | live Swank integration |
| `tests/mcp-e2e-test.lisp` | top-to-bottom MCP protocol coverage |
| `tests/hot-reload-test.lisp` | compile/load and reload behavior |
| `tests/process-manager-test.lisp` | managed Swank subprocess lifecycle |

Run the full suite with:

```lisp
(asdf:test-system :cl-tron-mcp)
```

## Common Change Recipes

### Add a New Tool

1. Implement the behavior in the relevant subsystem under `src/<area>/`.
2. Register the MCP-facing tool in `src/tools/<category>-tools.lisp`.
3. Ensure the symbol is exported from the package file for that subsystem.
4. Add or extend tests in `tests/`.
5. Add a tool reference page in `docs/tools/`.

### Change Startup Behavior

1. Update `start-mcp.sh` if the change affects CLI behavior or process startup.
2. Update `src/core/server.lisp` if the change affects transport orchestration.
3. Update `docs/starting-the-mcp.md` and `README.md`.

### Change Debugger / Hot Reload Behavior

1. Trace the relevant Swank RPC path in `src/swank/`.
2. Update the MCP wrapper in `src/tools/`.
3. Add regression tests in `tests/swank-integration-test.lisp`, `tests/mcp-e2e-test.lisp`, or `tests/hot-reload-test.lisp` as appropriate.

## Known Boundaries and Caveats

- The most reliable workflow is still **one long-running SBCL session with Swank**; Tron is a client of that session, not a replacement for it.
- `repl_*` tools are the higher-level API; `swank_*` tools expose lower-level Swank behavior.
- The local hot-reload fallback is useful for startup and non-connected scenarios, but the richest debugger and restart workflows require a live Swank connection.
- `run-mcp.sh` is a convenience wrapper for Nix/devenv users, not the canonical install path for ordinary Quicklisp users.
