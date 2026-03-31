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
