# CL-TRON-MCP Docker Container

A Docker container for running CL-TRON-MCP (SBCL Debugging MCP Server).

## Quick Start

### Run with Docker

```bash
# Build the image
docker build -t cl-tron-mcp .

# Run with stdio transport (for MCP clients)
docker run -it --rm cl-tron-mcp

# Run with HTTP transport on port 8080
docker run -p 8080:8080 -it --rm cl-tron-mcp \
  sbcl --non-interactive \
  --eval "(ql:quickload :cl-tron-mcp :silent t)" \
  --eval "(cl-tron-mcp/core:start-server :transport :http :port 8080)"
```

### Run with Docker Compose

```bash
# Start HTTP server on port 8080
docker compose up -d

# View logs
docker compose logs -f

# Stop
docker compose down
```

## Usage

### Stdio Transport (for MCP CLI clients)

```bash
docker run -it --rm cl-tron-mcp
```

The server will communicate via stdin/stdout using JSON-RPC 2.0.

### HTTP Transport

```bash
# Start server
docker run -d -p 8080:8080 --name cl-tron-mcp cl-tron-mcp

# Test health endpoint
curl http://localhost:8080/health

# Call a tool
curl -X POST http://localhost:8080/rpc \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

### Python Client

```python
from cl_tron_client import CLTronClient

# Connect to HTTP transport
client = CLTronClient(port=8080)
client.start()

# Use tools
result = client.inspect_function("CL:CAR")
print(result)

client.stop()
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MCP_PORT` | 8080 | HTTP server port |
| `MCP_TRANSPORT` | http | Transport type (stdio, http) |

## Examples

### Inspect a Function

```bash
curl -X POST http://localhost:8080/rpc \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "inspect_function",
      "arguments": {
        "symbolName": "CL:CAR"
      }
    }
  }'
```

### Evaluate Lisp Code

```bash
curl -X POST http://localhost:8080/rpc \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "repl_eval",
      "arguments": {
        "code": "(+ 10 20 30)",
        "package": "CL-USER"
      }
    }
  }'
```

## Development

### Build for Development

```bash
# Build with local sources mounted
docker build -t cl-tron-mcp:dev .

# Mount local project for development
docker run -it --rm \
  -v $(pwd):/app \
  -w /app \
  cl-tron-mcp:dev \
  sbcl --load tutorial-run.lisp
```

### Run Tests

```bash
docker run -it --rm cl-tron-mcp \
  sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run :cl-tron-mcp/tests)"
```

## Resources

- **SBCL Documentation**: http://www.sbcl.org/
- **Quicklisp**: https://www.quicklisp.org/
- **Model Context Protocol**: https://modelcontextprotocol.io/
- **CL-TRON-MCP GitHub**: https://github.com/anomalyco/cl-tron-mcp

## License

MIT License - see LICENSE file for details.
