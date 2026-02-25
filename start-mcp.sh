#!/usr/bin/env bash
# start-mcp.sh - Start CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode)
#
# CRITICAL for stdio: stdout must contain only newline-delimited JSON-RPC.
# - All pre-exec echo output is sent to stderr (>&2) so the client sees no banner.
# - For stdio we run SBCL with --noinform or ECL with -q so no Lisp banner is on stdout.
#
# Server Detection:
#   For HTTP/combined modes, checks if a server is already running via PID file
#   and health endpoint. If healthy, exits successfully without starting a new instance.
#
# Lisp selection (first match):
#   1. CLI: --use-sbcl or --use-ecl (error if that Lisp is not installed)
#   2. Auto: try sbcl, then ecl
#
# Usage:
#   ./start-mcp.sh                    # Combined (stdio + HTTP), default (recommended)
#   ./start-mcp.sh --stdio-only       # Stdio only (e.g. for MCP clients that start the server)
#   ./start-mcp.sh --http-only        # HTTP only on default port 4006
#   ./start-mcp.sh --http-only --port 9000
#   ./start-mcp.sh --use-sbcl         # Combined with SBCL
#   ./start-mcp.sh --websocket       # WebSocket on port 4006
#   ./start-mcp.sh --status           # Check if server is running
#   ./start-mcp.sh --stop             # Stop a running HTTP server
#   ./start-mcp.sh --help            # Show full options and examples
#
# Environment:
#   QUICKLISP_DIR  Optional; defaults to $HOME/quicklisp.
#
# Example OpenCode config (~/.config/opencode/opencode.json):
# {
#   "mcp": {
#     "cl-tron": {
#       "type": "local",
#       "command": ["~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh"],
#       "enabled": true
#     }
#   }
# }

set -e

# ============================================================================
# Configuration
# ============================================================================

export QUICKLISP_DIR="${QUICKLISP_DIR:-$HOME/quicklisp}"
PROOT="$(cd "$(dirname "$0")" && pwd)"
PID_FILE="$PROOT/.tron-server.pid"
HTTP_PORT_FILE="$PROOT/http-port.txt"
HEALTH_TIMEOUT=2

# Default: combined (stdio + HTTP) so both MCP and HTTP clients can connect
TRANSPORT="combined"
PORT="4006"
PORT_GIVEN=""
LISP_CHOICE=""
ACTION="start"

# HTTP/WebSocket default port (avoid 4005 which is usually Swank)
HTTP_DEFAULT_PORT="4006"

# ============================================================================
# Helper Functions
# ============================================================================

log_info() {
    echo "$1" >&2
}

log_error() {
    echo "ERROR: $1" >&2
}

log_debug() {
    [[ "${TRON_DEBUG:-}" == "1" ]] && echo "[DEBUG] $1" >&2
}

# Check if a process with given PID is running
is_process_running() {
    local pid="$1"
    [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null
}

# Check if port is in use
is_port_in_use() {
    local port="$1"
    local check_cmd
    if command -v ss &>/dev/null; then
        ss -tln 2>/dev/null | grep -q ":${port} "
    elif command -v netstat &>/dev/null; then
        netstat -tln 2>/dev/null | grep -q ":${port} "
    elif command -v lsof &>/dev/null; then
        lsof -i ":${port}" 2>/dev/null | grep -q LISTEN
    else
        # Fallback: try to connect
        nc -z 127.0.0.1 "$port" 2>/dev/null
    fi
}

# Check server health via HTTP endpoint
check_server_health() {
    local port="$1"
    local response
    
    if command -v curl &>/dev/null; then
        response=$(curl -s -m "$HEALTH_TIMEOUT" "http://127.0.0.1:${port}/health" 2>/dev/null) || return 1
    elif command -v wget &>/dev/null; then
        response=$(wget -q -O - -T "$HEALTH_TIMEOUT" "http://127.0.0.1:${port}/health" 2>/dev/null) || return 1
    else
        # No HTTP client available, assume healthy if port is open
        is_port_in_use "$port"
        return $?
    fi
    
    # Check for "ok" status in response
    echo "$response" | grep -q '"status".*"ok"' 2>/dev/null
}

# Read PID from file
get_server_pid() {
    if [[ -f "$PID_FILE" ]]; then
        cat "$PID_FILE" 2>/dev/null
    fi
}

# Write PID to file
write_pid_file() {
    local pid="$1"
    echo "$pid" > "$PID_FILE"
}

# Remove PID file
remove_pid_file() {
    rm -f "$PID_FILE"
}

# Get port from file or default
get_http_port() {
    if [[ -f "$HTTP_PORT_FILE" ]]; then
        cat "$HTTP_PORT_FILE" 2>/dev/null || echo "$HTTP_DEFAULT_PORT"
    else
        echo "$HTTP_DEFAULT_PORT"
    fi
}

# Check if server is already running (for HTTP/combined modes)
check_server_status() {
    local pid
    local port
    
    pid=$(get_server_pid)
    port=$(get_http_port)
    
    if [[ -n "$pid" ]] && is_process_running "$pid"; then
        # Process exists, check if it's healthy
        if check_server_health "$port"; then
            echo "running"
            return 0
        else
            echo "unhealthy"
            return 1
        fi
    elif is_port_in_use "$port"; then
        # Port is in use but no PID file - might be another instance
        if check_server_health "$port"; then
            echo "running-external"
            return 0
        else
            echo "port-in-use"
            return 1
        fi
    else
        echo "stopped"
        return 1
    fi
}

# Stop a running server
stop_server() {
    local pid
    local status
    
    pid=$(get_server_pid)
    status=$(check_server_status)
    
    if [[ "$status" == "stopped" ]]; then
        log_info "Server is not running."
        remove_pid_file
        return 0
    fi
    
    if [[ -n "$pid" ]] && is_process_running "$pid"; then
        log_info "Stopping server (PID: $pid)..."
        kill "$pid" 2>/dev/null || true
        # Wait for process to terminate
        local count=0
        while is_process_running "$pid" && [[ $count -lt 10 ]]; do
            sleep 1
            ((count++))
        done
        # Force kill if still running
        if is_process_running "$pid"; then
            log_info "Force killing server..."
            kill -9 "$pid" 2>/dev/null || true
        fi
    fi
    
    remove_pid_file
    log_info "Server stopped."
}

# Pre-flight checks
preflight_checks() {
    local errors=0
    
    # Check if QUICKLISP_DIR exists
    if [[ ! -d "$QUICKLISP_DIR" ]]; then
        log_error "Quicklisp directory not found: $QUICKLISP_DIR"
        log_error "Set QUICKLISP_DIR environment variable or install Quicklisp."
        ((errors++))
    elif [[ ! -f "$QUICKLISP_DIR/setup.lisp" ]]; then
        log_error "Quicklisp setup.lisp not found in: $QUICKLISP_DIR"
        ((errors++))
    fi
    
    # Check if cl-tron-mcp system exists
    if [[ ! -f "$PROOT/cl-tron-mcp.asd" ]]; then
        log_error "cl-tron-mcp.asd not found in: $PROOT"
        ((errors++))
    fi
    
    return $errors
}

# Detect available Lisp
detect_lisp() {
    if [[ "$LISP_CHOICE" == "sbcl" ]]; then
        if command -v sbcl &>/dev/null; then
            LISP="sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        elif [[ -x /usr/local/bin/sbcl ]]; then
            LISP="/usr/local/bin/sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        else
            log_error "SBCL not found. Install SBCL or do not use --use-sbcl."
            exit 1
        fi
    elif [[ "$LISP_CHOICE" == "ecl" ]]; then
        if command -v ecl &>/dev/null; then
            LISP="ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        elif [[ -x /usr/local/bin/ecl ]]; then
            LISP="/usr/local/bin/ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        else
            log_error "ECL not found. Install ECL or do not use --use-ecl."
            exit 1
        fi
    else
        # Auto-detect: sbcl first, then ecl
        if command -v sbcl &>/dev/null; then
            LISP="sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        elif [[ -x /usr/local/bin/sbcl ]]; then
            LISP="/usr/local/bin/sbcl"
            LISP_QUIET="--noinform"
            LISP_EVAL="--eval"
        elif command -v ecl &>/dev/null; then
            LISP="ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        elif [[ -x /usr/local/bin/ecl ]]; then
            LISP="/usr/local/bin/ecl"
            LISP_QUIET="-q"
            LISP_EVAL="-eval"
        else
            log_error "Neither SBCL nor ECL found. Install one of them."
            log_error "  Ubuntu/Debian: sudo apt install sbcl"
            log_error "  macOS: brew install sbcl"
            exit 1
        fi
    fi
    
    log_debug "Detected Lisp: $LISP"
}

# ============================================================================
# Argument Parsing
# ============================================================================

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
    --stdio-only)
        TRANSPORT="stdio"
        shift
        ;;
    --http-only)
        TRANSPORT="http"
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
    --status)
        ACTION="status"
        shift
        ;;
    --stop)
        ACTION="stop"
        shift
        ;;
    --help | -h)
        cat << 'HELP_EOF'
Usage: start-mcp.sh [OPTIONS]

Start the CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode).

Options:
  --use-sbcl       Use SBCL (error if not installed)
  --use-ecl        Use ECL (error if not installed)
  --stdio-only     Stdio only (no HTTP) - for MCP client communication
  --http-only      HTTP only (no stdio) - for persistent server
  --port PORT      HTTP/WebSocket port (default: 4006)
  --websocket      Use WebSocket transport
  --status         Check if server is running
  --stop           Stop a running HTTP server
  --help           Show this help

Default: combined (stdio + HTTP) so both MCP-over-stdio and HTTP clients can connect.

Server Detection:
  For HTTP/combined modes, the script checks if a server is already running.
  If a healthy server is found, it exits successfully without starting a new one.
  
  PID file: .tron-server.pid in the project directory

Examples:
  start-mcp.sh                      # Combined (stdio + HTTP on 4006)
  start-mcp.sh --stdio-only         # Stdio only (e.g. Cursor/OpenCode)
  start-mcp.sh --http-only          # HTTP only on port 4006
  start-mcp.sh --http-only --port 9000
  start-mcp.sh --status             # Check server status
  start-mcp.sh --stop               # Stop running server

MCP Client Config (Cursor ~/.cursor/mcp.json):
  {
    "mcpServers": {
      "tron": {
        "command": "~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh",
        "args": ["--stdio-only"]
      }
    }
  }

HELP_EOF
        exit 0
        ;;
    *)
        log_error "Unknown option: $1"
        exit 1
        ;;
    esac
done

# ============================================================================
# Main Logic
# ============================================================================

# Handle --status action
if [[ "$ACTION" == "status" ]]; then
    status=$(check_server_status)
    pid=$(get_server_pid)
    port=$(get_http_port)
    
    case "$status" in
    running)
        log_info "Server is RUNNING (PID: $pid, Port: $port)"
        exit 0
        ;;
    running-external)
        log_info "Server is RUNNING (external instance, Port: $port)"
        exit 0
        ;;
    unhealthy)
        log_info "Server is UNHEALTHY (PID: $pid, Port: $port)"
        exit 1
        ;;
    port-in-use)
        log_info "Port $port is IN USE by another process"
        exit 1
        ;;
    stopped)
        log_info "Server is STOPPED"
        exit 1
        ;;
    esac
fi

# Handle --stop action
if [[ "$ACTION" == "stop" ]]; then
    stop_server
    exit $?
fi

# For HTTP/combined modes, check if server is already running
if [[ "$TRANSPORT" != "stdio" ]]; then
    status=$(check_server_status)
    pid=$(get_server_pid)
    
    case "$status" in
    running)
        log_info "Server is already running (PID: $pid, Port: $(get_http_port))"
        log_info "Use 'start-mcp.sh --stop' to stop it first."
        exit 0
        ;;
    running-external)
        log_info "Server is already running on port $(get_http_port) (external instance)"
        exit 0
        ;;
    unhealthy)
        log_info "Server process exists but is unhealthy. Starting new instance..."
        remove_pid_file
        ;;
    port-in-use)
        log_error "Port $(get_http_port) is in use by another process."
        log_error "Use '--port <different-port>' or stop the conflicting process."
        exit 1
        ;;
    esac
fi

# Run pre-flight checks
if ! preflight_checks; then
    log_error "Pre-flight checks failed. Fix the issues above and try again."
    exit 1
fi

# Detect Lisp binary
detect_lisp

# HTTP port: when --port was not given, use 4006 for http or combined to avoid clashing with Swank (4005)
if [[ "$TRANSPORT" == "http" || "$TRANSPORT" == "combined" || "$TRANSPORT" == "websocket" ]]; then
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

log_info "Starting CL-TRON-MCP..."
log_info "  Lisp: $LISP"
log_info "  Transport: $TRANSPORT"
if [[ "$TRANSPORT" != "stdio" ]]; then
    log_info "  Port: $PORT"
fi
log_info ""
log_info "Available tools: 86"
log_info "  - Unified REPL (Swank)"
log_info "  - Inspector, Debugger, Profiler, Tracer"
log_info "  - Thread management, Monitoring"
log_info "  - Logging, Cross-reference, Security"
log_info ""

# Write port file for HTTP modes (used by health checks)
if [[ "$TRANSPORT" != "stdio" ]]; then
    echo "$PORT" > "$HTTP_PORT_FILE"
fi

# Function to clean up PID file on exit
cleanup() {
    local exit_code=$?
    if [[ "$TRANSPORT" != "stdio" ]] && [[ -f "$PID_FILE" ]]; then
        local pid_in_file=$(cat "$PID_FILE" 2>/dev/null)
        if [[ "$pid_in_file" == "$$" ]]; then
            remove_pid_file
        fi
    fi
    exit $exit_code
}

# For HTTP modes, set up PID file and cleanup trap
if [[ "$TRANSPORT" != "stdio" ]]; then
    trap cleanup EXIT
    write_pid_file "$$"
fi

# Build and execute the command
log_info ""
log_info "-------------------------------------------------------------------------"
log_info "--"
if [[ "$TRANSPORT" == "stdio" ]]; then
    log_info "-- Starting the stdio-only MCP"
    log_info "--"
    exec \
        "$LISP" \
        $LISP_QUIET \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(setq *compile-verbose* nil *load-verbose* nil)" \
        $LISP_EVAL "(push #p\"$PROOT/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :stdio-only) #+sbcl (sb-ext:quit 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
elif [[ "$TRANSPORT" == "combined" ]]; then
    log_info "-- Starting combined (stdio + HTTP on port $PORT)"
    log_info "--"
    exec \
        "$LISP" \
        $LISP_QUIET \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(setq *compile-verbose* nil *load-verbose* nil)" \
        $LISP_EVAL "(push #p\"$PROOT/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :combined :port $PORT) #+sbcl (sb-ext:quit 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
elif [[ "$TRANSPORT" == "http" ]]; then
    log_info "-- Starting the HTTP server on port $PORT"
    log_info "-- (Keep this running; use Ctrl+C to stop. If port $PORT is in use, e.g. by Swank, use --port N)"
    log_info '-- Test: curl -X POST http://127.0.0.1:'"$PORT"'/rpc -H "Content-Type: application/json" -d '"'"'{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'"'"''
    log_info "--"
    [[ "$LISP" = *sbcl* ]] && LISP_EXTRA="--non-interactive" || LISP_EXTRA=""
    # Write boot script (logs steps to /tmp/cl-tron-mcp-boot.log) then load it with one short --eval.
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
  (cl-tron-mcp/core:start-server :transport :http-only :port port))
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
    log_info "-- Starting Websocket server on port $PORT"
    log_info "--"
    [[ "$LISP" = *sbcl* ]] && LISP_EXTRA="--non-interactive" || LISP_EXTRA=""
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EXTRA \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(push #p\"$PROOT/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :websocket :port $PORT) #+sbcl (sb-ext:quit 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
fi
