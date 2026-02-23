#!/usr/bin/env bash
# start-mcp.sh - Start CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode)
#
# CRITICAL for stdio: stdout must contain only newline-delimited JSON-RPC.
# - All pre-exec echo output is sent to stderr (>&2) so the client sees no banner.
# - For stdio we run SBCL with --noinform or ECL with -q so no Lisp banner is on stdout.
#
# Lisp selection (first match):
#   1. CLI: --use-sbcl or --use-ecl (error if that Lisp is not installed)
#   2. Auto: try sbcl, then ecl
#
# Usage:
#   ./start-mcp.sh                    # Stdio, auto-detect Lisp (sbcl then ecl)
#   ./start-mcp.sh --use-sbcl          # Stdio, force SBCL
#   ./start-mcp.sh --use-ecl           # Stdio, force ECL
#   ./start-mcp.sh --http              # HTTP transport on default port 4006 (avoids Swank 4005)
#   ./start-mcp.sh --http --port 9000  # HTTP on port 9000
#   ./start-mcp.sh --websocket        # WebSocket on port 4006
#   ./start-mcp.sh --help              # Show full options and examples
#
# Environment:
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
PORT="4005"
PORT_GIVEN=""
LISP_CHOICE=""
# HTTP/WebSocket default port (avoid 4005 which is usually Swank)
HTTP_DEFAULT_PORT="4006"

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
        PORT_GIVEN=1
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
        echo "  --port PORT  HTTP/WebSocket port (default: 4006 for HTTP/WebSocket, to avoid Swank on 4005)"
        echo "  --websocket  Use WebSocket transport"
        echo "  --help       Show this help"
        echo ""
        echo "Lisp selection: --use-sbcl or --use-ecl (or auto-detect: sbcl, then ecl)."
        echo ""
        echo "Examples:"
        echo "  $0                      # Stdio (for OpenCode)"
        echo "  $0 --use-ecl             # Stdio with ECL"
        echo "  $0 --http                # HTTP on port 4006"
        echo "  $0 --http --port 9000    # HTTP on port 9000"
        echo "  $0 --websocket           # WebSocket on port 4006"
        exit 0
        ;;
    *)
        echo "Unknown option: $1" >&2
        exit 1
        ;;
    esac
done

# Detect Lisp: CLI (--use-sbcl/--use-ecl) > auto-detect (sbcl then ecl).
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

# HTTP/WebSocket: when --port was not given, use 4006 to avoid clashing with Swank (usually 4005)
if [[ "$TRANSPORT" == "http" || "$TRANSPORT" == "websocket" ]]; then
    if [[ -z "$PORT_GIVEN" ]]; then
        PORT="$HTTP_DEFAULT_PORT"
    fi
fi

# ECL does not load Quicklisp from init; load setup.lisp before any ql: use.
# Set *load-verbose* nil before load so ECL does not print to stdout (stdio = JSON only).
# Bind *standard-output* to *error-output* during load/compile so ECL style warnings go to stderr.
case "$LISP" in
*ecl*)
    ECL_LOAD_QL_FLAG="-eval"
    ECL_LOAD_QL_EXPR="(progn (setq *load-verbose* nil *compile-verbose* nil) (let ((*standard-output* *error-output*)) (load #p\"$QUICKLISP_DIR/setup.lisp\")))"
    COMPILE_EXPR="(let ((*standard-output* *error-output*)) (asdf:compile-system :cl-tron-mcp :force t))"
    LOAD_EXPR="(let ((*standard-output* *error-output*)) (asdf:load-system :cl-tron-mcp))"
    ;;
*)
    ECL_LOAD_QL_FLAG=""
    ECL_LOAD_QL_EXPR=""
    COMPILE_EXPR="(asdf:compile-system :cl-tron-mcp :force t)"
    LOAD_EXPR="(asdf:load-system :cl-tron-mcp)"
    ;;
esac

# Do not pass empty args to Lisp (e.g. SBCL treats '' as a script and exits before --eval).
ECL_ARGS=()
[[ -n "$ECL_LOAD_QL_FLAG" ]] && ECL_ARGS=("$ECL_LOAD_QL_FLAG" "$ECL_LOAD_QL_EXPR")

echo "Starting CL-TRON-MCP..." >&2
echo "  Lisp: $LISP" >&2
echo "  Transport: $TRANSPORT" >&2
if [[ "$TRANSPORT" != "stdio" ]]; then
    echo "  Port: $PORT" >&2
fi
echo "" >&2
echo "Available tools: 86" >&2
echo "  - Unified REPL (Swank)" >&2
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
    # LISP_QUIET: SBCL --noinform / ECL -q so stdout = JSON-RPC only for MCP clients.
    # After start-server returns (client disconnected), quit so we never run the REPL on stdin.
    exec \
        "$LISP" \
        $LISP_QUIET \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(setq *compile-verbose* nil *load-verbose* nil)" \
        $LISP_EVAL "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :stdio) #+sbcl (sb-ext:quit 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
elif [[ "$TRANSPORT" == "http" ]]; then
    echo "-- Starting the HTTP server on port $PORT" >&2
    echo "-- (Keep this running; use Ctrl+C to stop. If port $PORT is in use, e.g. by Swank, use --port N)" >&2
    echo '-- Test: curl -X POST http://127.0.0.1:'"$PORT"'/rpc -H "Content-Type: application/json" -d '"'"'{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'"'"'' >&2
    echo "--" >&2
    echo "$PORT" > http-port.txt
    [[ "$LISP" = *sbcl* ]] && LISP_EXTRA="--non-interactive" || LISP_EXTRA=""
    # Write boot script (logs steps to /tmp/cl-tron-mcp-boot.log) then load it with one short --eval.
    PROOT="$(pwd)"
    BOOT_FILE="$PROOT/scripts/http-boot.lisp"
    cat > "$BOOT_FILE" << BOOTEOF
(defun %boot-log (msg)
  (ignore-errors
    (with-open-file (f #p"/tmp/cl-tron-mcp-boot.log" :direction :output :if-exists :append :if-does-not-exist :create)
      (write-line (format nil "~a ~a" (get-universal-time) msg) f))))
(%boot-log "0: entered")
(setq *compile-verbose* nil *load-verbose* nil)
(%boot-log "1: setq done")
(push #p"$PROOT/" ql:*local-project-directories*)
(%boot-log "2: push done")
(asdf:compile-system :cl-tron-mcp :force t)
(%boot-log "3: compile done")
(asdf:load-system :cl-tron-mcp)
(%boot-log "4: load done")
(let ((port (parse-integer (with-open-file (f #p"$PROOT/http-port.txt") (read-line f)))))
  (%boot-log (format nil "5: port=~a calling start-server" port))
  (cl-tron-mcp/core:start-server :transport :http :port port))
(%boot-log "6: start-server returned")
#+sbcl (sb-ext:quit 0)
#+ecl (ext:quit 0)
#-(or sbcl ecl) (cl-user::quit 0)
BOOTEOF
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EXTRA \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(load #p\"$BOOT_FILE\")"
elif [[ "$TRANSPORT" == "websocket" ]]; then
    echo "-- Starting Websocket server on port $PORT" >&2
    echo "--" >&2
    [[ "$LISP" = *sbcl* ]] && LISP_EXTRA="--non-interactive" || LISP_EXTRA=""
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EXTRA \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(push #p\"$(pwd)/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :websocket :port $PORT) #+sbcl (sb-ext:quit 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
fi
