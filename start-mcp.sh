#!/usr/bin/env bash
# start-mcp.sh - Start CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode)
#
# CRITICAL for stdio: stdout must contain only newline-delimited JSON-RPC.
# - All pre-exec echo output is sent to stderr (>&2) so the client sees no banner.
# - For stdio we run SBCL with --noinform so the SBCL banner is not on stdout.
#
# Usage:
#   ./start-mcp.sh              # Start with stdio (default, for MCP clients)
#   ./start-mcp.sh --http      # Start with HTTP transport
#   ./start-mcp.sh --port 8080 # Custom port for HTTP
#
# Example OpenCode config (~/.config/opencode/opencode.json):
# {
#   "mcp": {
#     "cl-tron": {
#       "type": "local",
#       "command": ["/path/to/cl-tron/start-mcp.sh"],
#       "enabled": true
#     }
#   }
# }

set -e

export QUICKLISP_DIR="$HOME/quicklisp"

# Navigate to the cl-tron-mcp directory
cd "$(dirname "$0")"

# Detect SBCL
if command -v sbcl &>/dev/null; then
    SBCL="sbcl"
elif command -v /usr/local/bin/sbcl &>/dev/null; then
    SBCL="/usr/local/bin/sbcl"
else
    echo "Error: SBCL not found. Please install SBCL first."
    exit 1
fi

# Default settings
TRANSPORT="stdio"
PORT="8080"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    --http)
        TRANSPORT="http"
        shift
        ;;
    --port)
        PORT="$2"
        shift 2
        ;;
    --websocket)
        TRANSPORT="websocket"
        shift
        ;;
    --help | -h)
        echo "Usage: $0 [--http] [--port PORT] [--websocket]"
        echo ""
        echo "Options:"
        echo "  --http       Use HTTP transport (default: stdio)"
        echo "  --port PORT  HTTP/WebSocket port (default: 8080)"
        echo "  --websocket  Use WebSocket transport"
        echo "  --help       Show this help"
        echo ""
        echo "Examples:"
        echo "  $0                      # Stdio (for OpenCode)"
        echo "  $0 --http               # HTTP on port 8080"
        echo "  $0 --http --port 9000   # HTTP on port 9000"
        echo "  $0 --websocket          # WebSocket on port 8080"
        exit 0
        ;;
    *)
        echo "Unknown option: $1"
        exit 1
        ;;
    esac
done

echo "Starting CL-TRON-MCP..." >&2
echo "  SBCL: $SBCL" >&2
echo "  Transport: $TRANSPORT" >&2
if [[ "$TRANSPORT" != "stdio" ]]; then
    echo "  Port: $PORT" >&2
fi
echo "" >&2
echo "Available tools: 100" >&2
echo "  - Unified REPL (auto-detects Swank/nrepl)" >&2
echo "  - Inspector, Debugger, Profiler, Tracer" >&2
echo "  - Thread management, Monitoring" >&2
echo "  - Logging, Cross-reference, Security" >&2
echo "" >&2

# Build and execute the command
echo "" >&2
echo "-------------------------------------------------------------------------" >&2
echo "--" >&2
if [[ "$TRANSPORT" == "stdio" ]]; then
    echo "-- Starting the stdio MCP" >&2
    echo "--" >&2
    # --noinform: suppress SBCL banner so stdout = JSON-RPC only for MCP clients
    # exec "$SBCL" --noinform --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :stdio)"
    exec \
        "$SBCL" \
        --noinform \
        --eval "(setq *compile-verbose* nil *load-verbose* nil)" \
        --eval "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        --eval "(asdf:compile-system :cl-tron-mcp :force t)" \
        --eval "(asdf:load-system :cl-tron-mcp)" \
        --eval "(cl-tron-mcp/core:start-server :transport :stdio)"
elif [[ "$TRANSPORT" == "http" ]]; then
    echo "-- Starting the HTTP server on port $PORT" >&2
    echo "--" >&2
    # exec "$SBCL" --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :http :port $PORT)"
    exec \
        "$SBCL" \
        --eval "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        --eval "(asdf:compile-system :cl-tron-mcp :force t)" \
        --eval "(asdf:load-system :cl-tron-mcp)" \
        --eval "(cl-tron-mcp/core:start-server :transport :http :port $PORT)"
elif [[ "$TRANSPORT" == "websocket" ]]; then
    echo "-- Starting Websocket server on port $PORT" >&2
    echo "--" >&2
    # exec "$SBCL" --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :websocket :port $PORT)"
    exec \
        "$SBCL" \
        --eval "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        --eval "(asdf:compile-system :cl-tron-mcp :force t)" \
        --eval "(asdf:load-system :cl-tron-mcp)" \
        --eval "(cl-tron-mcp/core:start-server :transport :websocket :port $PORT)"
fi
