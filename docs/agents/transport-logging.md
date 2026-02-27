# Transport & Logging

## Transport Modes

The MCP server supports multiple transport modes for different use cases.

### Combined Mode (Default)

Runs both stdio and HTTP transports simultaneously:

```lisp
(cl-tron-mcp/core:start-server)
```

- **Stdio**: For MCP clients (Cursor, Kilocode, Opencode)
- **HTTP**: For web clients and testing on port 4006

### Stdio-Only Mode

For MCP clients that start the server directly:

```lisp
(cl-tron-mcp/core:start-server :transport :stdio-only)
```

Or via script:

```bash
./start-mcp.sh --stdio-only
```

### HTTP-Only Mode

For web clients and manual testing:

```lisp
(cl-tron-mcp/core:start-server :transport :http-only :port 4006)
```

Or via script:

```bash
./start-mcp.sh --http-only --port 4006
```

### WebSocket Mode

Placeholder implementation (not fully functional):

```lisp
(cl-tron-mcp/core:start-server :transport :websocket :port 23456)
```

## Stdio Transport Requirements

**Critical**: MCP over stdio expects stdout to contain only newline-delimited JSON-RPC messages.

### Rules

- **Do not write to stdout** except JSON responses
- No banners, no `[MCP]` messages, no SBCL startup text
- All server activity must be logged via log4cl to stderr
- Handlers return already-serialized JSON strings

### SBCL Startup for Stdio

Use `--noinform` to suppress SBCL banner:

```bash
sbcl --non-interactive --noinform \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
```

## HTTP Transport

Implemented with Hunchentoot.

### Endpoints

- **POST** `/rpc` or `/mcp` - JSON-RPC requests
- **GET** `/health` - Server health check
- **GET** `/lisply/ping-lisp` - Lisp connectivity test
- **GET** `/lisply/tools/list` - List available tools

### Default Port

- **4006** (to avoid conflict with Swank on 4005)

## Logging

Use log4cl for all logging, not direct output.

### Log Levels

```lisp
(cl-tron-mcp/logging:log-info "Server started")
(cl-tron-mcp/logging:log-warn "Connection timeout")
(cl-tron-mcp/logging:log-error "Failed to compile")
(cl-tron-mcp/logging:log-debug "Frame details: ~a" frame)
```

### Stdio Configuration

When using stdio transport, log4cl is configured to write to stderr:

```lisp
(cl-tron-mcp/logging:ensure-log-to-stream *error-output*)
```

### Package-Level Logging

Configure logging per package:

```lisp
(cl-tron-mcp/logging:log-configure :cl-tron-mcp/swank :warn)
(cl-tron-mcp/logging:log-configure :cl-tron-mcp/unified :info)
```

## MCP Protocol Compliance

### Initialize Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "resources": {
        "subscribe": false
      }
    },
    "serverInfo": {
      "name": "cl-tron-mcp",
      "version": "0.1.0"
    }
  }
}
```

### Prompts Format

Prompts return messages with content as an array of parts:

```json
{
  "messages": [
    {
      "role": "user",
      "content": [
        {
          "type": "text",
          "text": "Step 1: Connect to Swank..."
        }
      ]
    }
  ]
}
```

Use lowercase keys for MCP compatibility.

## Connection Examples

### Manual Test (Stdio)

```bash
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
```

### Manual Test (HTTP)

```bash
curl -X POST http://127.0.0.1:4006/rpc \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}'
```

## Troubleshooting Transport Issues

| Symptom | Cause | Solution |
|---------|-------|----------|
| Client fails to parse | Non-JSON on stdout | Ensure `--noinform` and no `format t` |
| No response | Thread crash | Use stdio transport, check logs |
| Port in use | Conflicting process | Use different port or kill process |
| HTTP 404 | Wrong endpoint | Use `/rpc` or `/mcp` |

## See Also

- [Getting Started](getting-started.md) - Connection setup
- [Troubleshooting](troubleshooting.md) - Common issues
- [Workflows](workflows.md) - Usage patterns