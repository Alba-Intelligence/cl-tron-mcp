# cl-tron-mcp Roadmap

This document tracks planned and in-progress features for future versions of cl-tron-mcp.

## Near-term (next release)

### WebSocket Transport

Implement full WebSocket transport to complement stdio and HTTP:

- RFC 6455 handshake and framing (upgrade from HTTP)
- Bi-directional streaming for MCP notifications
- Likely via `cl-websocket` or `hunchentoot-websocket`

### Custom Validators

Allow tool authors to register domain-specific validators:

- `define-custom-validator` macro + validator registry
- Composable validators (e.g., `(and validate-string validate-url)`)

### Metrics Collection

Add request-level metrics for production monitoring:

- Latency histograms per tool
- Error rate tracking
- Tool usage counters
- Export as Prometheus-compatible text or JSON

### Request Tracing

Add correlation IDs across the request lifecycle:

- JSON-RPC request → security check → tool execution → response
- Configurable trace output (stderr / log4cl)

### Array / Sequence Validation

Expand `validate-list` with:

- Min/max length constraints
- Per-element validators with index-level error messages

## Tool gaps found by the 2026-11-10 documentation audit

Older docs described tools that are **not registered**. They were removed from user-facing docs; the
useful ones are recorded here. "Today" says what exists. Names are proposals and must follow the
`repl_*` / existing naming; each needs a registry entry, tests, a `docs/tools/` page, and (if it mutates
the image) `:requires-approval t`.

| Proposed tool | Replaces doc mention | Today | Notes |
|---|---|---|---|
| `code_replace_function` | `code_replace_function` | `cl-tron-mcp/hot-reload:replace-function` exists (local fallback only), no tool | Needs the Swank path; atomic replace with rollback on failure. |
| `code_reload_package` | `code_reload_package` | `reload-package` exists but cannot find source files and now returns an error | Needs a package -> ASDF system/file mapping (`sb-introspect`). |
| `code_load_fasl`, `code_compile_file` | same | none | Load/compile a file in the target image; approval required. |
| `source_location` | `source_location` | `get-source-location` is a stub | Use `sb-introspect:find-definition-source` in the target image. |
| `repl_eval_in_frame` | `debugger_eval_in_frame` | Swank supports it; no tool | Evaluate in a frame's lexical environment; approval required. |
| `repl_list_packages`, `repl_list_symbols`, `inspect_package` filters | `list_packages`, `list_symbols` | `inspect_package`, `repl_completions` | Maybe unnecessary; evaluate need first. |
| `who_exports`, `who_specializes`, `list_classes` | same | `who_calls`, `who_references`, `who_binds`, `who_sets`, `list_callees`, `inspect_class` | Add via `sb-introspect` / MOP. |
| `memory_stats`, `gc_stats` | same | `gc_run`, `runtime_stats`, `health_check` | Heap by generation, GC time and counts from `sb-ext`/`sb-kernel`. |
| `metrics_export` | `metrics_export` | `src/tracer/metrics.lisp` records per-tool latency internally | Expose snapshot as JSON and Prometheus text. |
| `profile_reset`, `profile_flamegraph` | same | `profile_start/stop/report`; `sprof-start`/`sprof-report` exist as functions, not tools | Statistical profiling tool plus flamegraph export. |
| `thread_kill` | `thread_kill` | `repl_abort`, `swank_interrupt`, `swank_kill` (managed process) | Terminate a named thread in the target; approval required. |
| `request_list`, `request_status`, `request_cancel` | same (in error docs) | none; tool calls are synchronous and serial | Needs async tool execution with real timeouts (review finding B4). |

## Medium-term

### Multiple SBCL Session Support

Allow connecting to more than one SBCL+Swank session simultaneously:

- Named sessions (`swank_connect :session "prod"`)
- Session-scoped tool calls

### MCP Notifications

Push asynchronous events to connected MCP clients:

- Debugger entered / exited
- Output from long-running evaluations
- System health alerts

### Breakpoint Persistence

Persist breakpoints across hot reloads:

- Save to `.cl-tron-breakpoints.json`
- Restore on system reload

### Session Recording / Replay

Record evaluation sessions for later replay or sharing:

- Capture inputs, outputs, and debugger events
- Playback for regression testing

## Long-term

### Multi-implementation Support

Extend beyond SBCL to other Common Lisp implementations:

- Clozure CL (CCL) — partial `#+ccl` guards already in place
- ABCL (JVM Lisp)
- ECL (embeddable CL)

### Web UI

Optional browser-based debugging interface:

- Connected to HTTP transport
- Live evaluation, backtraces, inspector views

### VS Code Extension

Native VS Code extension wrapping the MCP server:

- IntelliSense for CL symbols via autocomplete tool
- Inline debugger via DAP protocol adapter

## Completed

See [CHANGELOG.md](../../CHANGELOG.md) for completed improvements.
