#!/usr/bin/env bash
# start-mcp.sh - Start CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode)
#
# CRITICAL for stdio: stdout must contain only newline-delimited JSON-RPC.
# - All pre-exec echo output is sent to stderr (>&2) so the client sees no banner.
# - For stdio we run SBCL with --noinform or ECL with -q so no Lisp banner is on stdout.
#
# Lisp selection (first match):
#   1. CLI: --use-sbcl or --use-ecl (error if that Lisp is not installed)
#   2. Env: TRON_LISP=sbcl or TRON_LISP=ecl
#   3. Auto: try sbcl, then ecl
#
# Usage:
#   ./start-mcp.sh                    # Stdio, auto-detect Lisp (sbcl then ecl)
#   ./start-mcp.sh --use-sbcl          # Stdio, force SBCL
#   ./start-mcp.sh --use-ecl           # Stdio, force ECL
#   ./start-mcp.sh --http              # HTTP transport on default port 8080
#   ./start-mcp.sh --http --port 9000  # HTTP on port 9000
#   ./start-mcp.sh --websocket        # WebSocket on port 8080
#   ./start-mcp.sh --help              # Show full options and examples
#
# Environment:
#   TRON_LISP   When set to sbcl or ecl, select that Lisp (unless --use-sbcl/--use-ecl is given).
#   QUICKLISP_DIR  Optional; defaults to $HOME/quicklisp.
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

# Default settings
TRANSPORT="stdio"
PORT="8080"
LISP_CHOICE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    --use-sbcl)
        LISP_CHOICE="sbcl"
        shift
        ;;
    --use-ecl)
        LISP_CHOICE="ecl"
        shift
        ;;
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
        echo "Usage: $0 [--use-sbcl | --use-ecl] [--http] [--port PORT] [--websocket]"
        echo ""
        echo "Options:"
        echo "  --use-sbcl   Use SBCL (error if not installed)"
        echo "  --use-ecl    Use ECL (error if not installed)"
        echo "  --http       Use HTTP transport (default: stdio)"
        echo "  --port PORT  HTTP/WebSocket port (default: 8080)"
        echo "  --websocket  Use WebSocket transport"
        echo "  --help       Show this help"
        echo ""
        echo "Lisp selection (first match): --use-sbcl/--use-ecl, then TRON_LISP, then auto-detect (sbcl, then ecl)."
        echo "Environment: TRON_LISP=sbcl or TRON_LISP=ecl to prefer a Lisp when no --use-* option is given."
        echo ""
        echo "Examples:"
        echo "  $0                      # Stdio (for OpenCode)"
        echo "  $0 --use-ecl             # Stdio with ECL"
        echo "  $0 --http                # HTTP on port 8080"
        echo "  $0 --http --port 9000    # HTTP on port 9000"
        echo "  $0 --websocket           # WebSocket on port 8080"
        exit 0
        ;;
    *)
        echo "Unknown option: $1" >&2
        exit 1
        ;;
    esac
done

# Detect Lisp: CLI (--use-sbcl/--use-ecl) > TRON_LISP > auto-detect (sbcl then ecl).
if [ "$LISP_CHOICE" = "sbcl" ]; then
    if command -v sbcl &>/dev/null; then
        LISP="sbcl"
        LISP_QUIET="--noinform"
        LISP_EVAL="--eval"
    elif command -v /usr/local/bin/sbcl &>/dev/null; then
        LISP="/usr/local/bin/sbcl"
        LISP_QUIET="--noinform"
        LISP_EVAL="--eval"
    else
        echo "Error: SBCL not found. Install SBCL or do not use --use-sbcl." >&2
        exit 1
    fi
elif [ "$LISP_CHOICE" = "ecl" ]; then
    if command -v ecl &>/dev/null; then
        LISP="ecl"
        LISP_QUIET="-q"
        LISP_EVAL="-eval"
    elif command -v /usr/local/bin/ecl &>/dev/null; then
        LISP="/usr/local/bin/ecl"
        LISP_QUIET="-q"
        LISP_EVAL="-eval"
    else
        echo "Error: ECL not found. Install ECL or do not use --use-ecl." >&2
        exit 1
    fi
elif [ -n "$TRON_LISP" ]; then
    if [ "$TRON_LISP" = "sbcl" ]; then
        if command -v sbcl &>/dev/null; then
            LISP="sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        elif command -v /usr/local/bin/sbcl &>/dev/null; then
            LISP="/usr/local/bin/sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        else
            echo "Error: TRON_LISP=sbcl but SBCL not found." >&2
            exit 1
        fi
    elif [ "$TRON_LISP" = "ecl" ]; then
        if command -v ecl &>/dev/null; then
            LISP="ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        elif command -v /usr/local/bin/ecl &>/dev/null; then
            LISP="/usr/local/bin/ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        else
            echo "Error: TRON_LISP=ecl but ECL not found." >&2
            exit 1
        fi
    else
        echo "Error: TRON_LISP must be 'sbcl' or 'ecl' (got: $TRON_LISP)." >&2
        exit 1
    fi
else
    # Auto-detect: sbcl first, then ecl
    if command -v sbcl &>/dev/null; then
        LISP="sbcl"
        LISP_QUIET="--noinform"
        LISP_EVAL="--eval"
    elif command -v /usr/local/bin/sbcl &>/dev/null; then
        LISP="/usr/local/bin/sbcl"
        LISP_QUIET="--noinform"
        LISP_EVAL="--eval"
    elif command -v ecl &>/dev/null; then
        LISP="ecl"
        LISP_QUIET="-q"
        LISP_EVAL="-eval"
    elif command -v /usr/local/bin/ecl &>/dev/null; then
        LISP="/usr/local/bin/ecl"
        LISP_QUIET="-q"
        LISP_EVAL="-eval"
    else
        echo "Error: SBCL or ECL not found. Install one or use --use-sbcl / --use-ecl." >&2
        exit 1
    fi
fi

echo "Starting CL-TRON-MCP..." >&2
echo "  Lisp: $LISP" >&2
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
    # LISP_QUIET: SBCL --noinform / ECL -q so stdout = JSON-RPC only for MCP clients
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EVAL "(setq *compile-verbose* nil *load-verbose* nil)" \
        $LISP_EVAL "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        $LISP_EVAL "(asdf:compile-system :cl-tron-mcp :force t)" \
        $LISP_EVAL "(asdf:load-system :cl-tron-mcp)" \
        $LISP_EVAL "(cl-tron-mcp/core:start-server :transport :stdio)"
elif [[ "$TRANSPORT" == "http" ]]; then
    echo "-- Starting the HTTP server on port $PORT" >&2
    echo "--" >&2
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EVAL "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        $LISP_EVAL "(asdf:compile-system :cl-tron-mcp :force t)" \
        $LISP_EVAL "(asdf:load-system :cl-tron-mcp)" \
        $LISP_EVAL "(cl-tron-mcp/core:start-server :transport :http :port $PORT)"
elif [[ "$TRANSPORT" == "websocket" ]]; then
    echo "-- Starting Websocket server on port $PORT" >&2
    echo "--" >&2
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EVAL "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        $LISP_EVAL "(asdf:compile-system :cl-tron-mcp :force t)" \
        $LISP_EVAL "(asdf:load-system :cl-tron-mcp)" \
        $LISP_EVAL "(cl-tron-mcp/core:start-server :transport :websocket :port $PORT)"
fi
