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
# Session Management:
#   The PID file contains JSON metadata: pid, port, transport, started, user, command, ppid.
#   Use --restart to stop an existing instance and start a new one.
#
# Transport Modes:
#   - stdio-only: Short-lived, exits when MCP client disconnects
#   - http-only: Long-running HTTP server
#   - combined: Long-running HTTP server (stdio handled via HTTP client pattern)
#
# Lisp selection (first match):
#   1. CLI: --use-sbcl or --use-ecl (error if that Lisp is not installed)
#   2. Auto: try sbcl, then ecl
#
# Usage:
#   ./start-mcp.sh                    # Combined (long-running HTTP), default
#   ./start-mcp.sh --stdio-only       # Stdio only (short-lived, for MCP clients)
#   ./start-mcp.sh --http-only        # HTTP only on default port 4006
#   ./start-mcp.sh --http-only --port 9000
#   ./start-mcp.sh --use-sbcl         # Combined with SBCL
#   ./start-mcp.sh --status           # Check if server is running
#   ./start-mcp.sh --stop             # Stop a running server (graceful)
#   ./start-mcp.sh --stop --force     # Force kill a non-responsive server
#   ./start-mcp.sh --kill-port 4006   # Kill any process on port 4006
#   ./start-mcp.sh --restart          # Stop existing and start new
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

# Default: combined (long-running HTTP server)
TRANSPORT="combined"
PORT="4006"
PORT_GIVEN=""
LISP_CHOICE=""
ACTION="start"
FORCE_STOP=false

# HTTP/WebSocket default port (avoid 4005 which is usually Swank)
HTTP_DEFAULT_PORT="4006"

# Grace period for graceful shutdown (seconds)
GRACE_PERIOD=10

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
    [[ "${TRON_DEBUG:-}" == "1" ]] && echo "[DEBUG] $1" >&2 || true
}

log_warn() {
    echo "WARNING: $1" >&2
}

# Check if a process with given PID is running
is_process_running() {
    local pid="$1"
    [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null
}

# Check if port is in use
is_port_in_use() {
    local port="$1"
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

# ============================================================================
# PID File Management (Enhanced JSON format)
# ============================================================================

# Get current timestamp (seconds since epoch)
get_timestamp() {
    date +%s
}

# Generate a unique ID for this server instance
generate_unique_id() {
    if command -v uuidgen &>/dev/null; then
        uuidgen | tr -d '-'
    elif [[ -f /proc/sys/kernel/random/uuid ]]; then
        cat /proc/sys/kernel/random/uuid | tr -d '-'
    else
        # Fallback: use timestamp + random
        echo "$(date +%s)$RANDOM"
    fi
}

# Read PID file and extract field
get_pid_field() {
    local field="$1"
    if [[ -f "$PID_FILE" ]]; then
        local content=$(cat "$PID_FILE" 2>/dev/null)
        # Simple JSON parsing without jq dependency
        echo "$content" | grep -o "\"$field\"[[:space:]]*:[[:space:]]*[^,}]*" | sed 's/.*:[[:space:]]*//' | tr -d '",'
    fi
}

# Get PID from file
get_server_pid() {
    get_pid_field "pid"
}

# Get port from PID file
get_pid_port() {
    get_pid_field "port"
}

# Get transport from PID file
get_pid_transport() {
    get_pid_field "transport"
}

# Get started timestamp from PID file
get_pid_started() {
    get_pid_field "started"
}

# Get user from PID file
get_pid_user() {
    get_pid_field "user"
}

# Get command from PID file
get_pid_command() {
    get_pid_field "command"
}

# Get unique_id from PID file
get_pid_unique_id() {
    get_pid_field "unique_id"
}

# Get parent PID from PID file
get_pid_ppid() {
    get_pid_field "ppid"
}

# Write PID file with enhanced JSON metadata
write_pid_file() {
    local pid="$1"
    local port="$2"
    local transport="$3"
    local started=$(get_timestamp)
    local user="${USER:-$(whoami 2>/dev/null || echo 'unknown')}"
    local ppid="$$"
    local unique_id=$(generate_unique_id)
    
    # Store first few chars of command for verification (truncate to avoid JSON issues)
    local cmd_short="${0##*/} (transport: $transport, port: $port)"
    
    cat > "$PID_FILE" << EOF
{
  "pid": $pid,
  "port": $port,
  "transport": "$transport",
  "started": $started,
  "user": "$user",
  "command": "$cmd_short",
  "ppid": $ppid,
  "unique_id": "$unique_id"
}
EOF
    log_debug "Wrote PID file: pid=$pid, port=$port, transport=$transport, user=$user, unique_id=$unique_id"
}

# Remove PID file
remove_pid_file() {
    rm -f "$PID_FILE"
    log_debug "Removed PID file"
}

# Get port from file or default
get_http_port() {
    if [[ -f "$HTTP_PORT_FILE" ]]; then
        cat "$HTTP_PORT_FILE" 2>/dev/null || echo "$HTTP_DEFAULT_PORT"
    else
        echo "$HTTP_DEFAULT_PORT"
    fi
}

# ============================================================================
# Process Verification (Safety Checks)
# ============================================================================

# Verify that a PID is actually our server process
verify_process() {
    local pid="$1"
    local stored_user="$2"
    local stored_command="$3"
    
    # Check if process exists
    if ! is_process_running "$pid"; then
        log_debug "Process $pid does not exist"
        return 1
    fi
    
    # Check if we can read /proc (Linux)
    if [[ -d "/proc/$pid" ]]; then
        # Get actual process command line
        local actual_cmd=""
        if [[ -f "/proc/$pid/cmdline" ]]; then
            actual_cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null | head -c 200)
        fi
        
        # Get process user
        local actual_user=""
        if [[ -f "/proc/$pid/status" ]]; then
            actual_user=$(grep "^Uid:" "/proc/$pid/status" 2>/dev/null | awk '{print $3}')
            # Convert UID to username if possible
            if [[ -n "$actual_user" ]] && command -v id &>/dev/null; then
                actual_user=$(id -nu "$actual_user" 2>/dev/null || echo "$actual_user")
            fi
        fi
        
        log_debug "Verification: pid=$pid, stored_user=$stored_user, actual_user=$actual_user"
        log_debug "Verification: cmd='$actual_cmd', stored='$stored_command'"
        
        # Verify it's a Lisp process (sbcl, ecl, or similar)
        if [[ "$actual_cmd" == *"sbcl"* ]] || [[ "$actual_cmd" == *"ecl"* ]] || \
           [[ "$actual_cmd" == *"cl-tron"* ]] || [[ -z "$stored_command" ]]; then
            # Accept if it looks like our process or if we have no stored command
            return 0
        fi
        
        # If user matches, trust it
        if [[ -n "$stored_user" ]] && [[ "$stored_user" == "$actual_user" ]]; then
            return 0
        fi
        
        log_warn "Process $pid verification failed - may not be our server"
        log_warn "  Expected user: $stored_user, Actual: $actual_user"
        return 1
    fi
    
    # For non-Linux or if /proc unavailable, just check if process exists
    log_debug "Cannot verify process $pid via /proc, relying on PID existence"
    return 0
}

# ============================================================================
# Port-Based Process Discovery
# ============================================================================

# Find PID of process listening on a specific port
find_pid_by_port() {
    local port="$1"
    local pid=""
    
    # Try lsof first (most reliable)
    if command -v lsof &>/dev/null; then
        pid=$(lsof -t -i ":${port}" 2>/dev/null | head -1)
        if [[ -n "$pid" ]]; then
            echo "$pid"
            return 0
        fi
    fi
    
    # Try ss
    if command -v ss &>/dev/null; then
        pid=$(ss -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)
        if [[ -n "$pid" ]]; then
            echo "$pid"
            return 0
        fi
    fi
    
    # Try netstat
    if command -v netstat &>/dev/null; then
        pid=$(netstat -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)
        if [[ -n "$pid" ]]; then
            echo "$pid"
            return 0
        fi
    fi
    
    return 1
}

# Kill process by port (for orphaned servers)
kill_by_port() {
    local port="$1"
    local force="$2"
    local pid
    
    pid=$(find_pid_by_port "$port")
    
    if [[ -z "$pid" ]]; then
        log_info "No process found listening on port $port"
        return 1
    fi
    
    log_info "Found process $pid listening on port $port"
    
    if [[ "$force" == "true" ]]; then
        log_info "Force killing process $pid..."
        kill -9 "$pid" 2>/dev/null || true
    else
        log_info "Sending SIGTERM to process $pid..."
        kill "$pid" 2>/dev/null || true
        
        # Wait for graceful termination
        local count=0
        while is_process_running "$pid" && [[ $count -lt "$GRACE_PERIOD" ]]; do
            sleep 1
            ((count++))
        done
        
        if is_process_running "$pid"; then
            log_info "Process $pid did not terminate gracefully, force killing..."
            kill -9 "$pid" 2>/dev/null || true
        fi
    fi
    
    # Verify port is now free
    if ! is_port_in_use "$port"; then
        log_info "Port $port is now free"
        return 0
    else
        log_error "Port $port is still in use after kill attempt"
        return 1
    fi
}

# ============================================================================
# Server Status and Stop Functions
# ============================================================================

# Check if server is already running (for HTTP/combined modes)
check_server_status() {
    local pid
    local port
    
    pid=$(get_server_pid)
    port=$(get_pid_port)
    
    # If no PID file port, use http-port.txt or default
    if [[ -z "$port" ]]; then
        port=$(get_http_port)
    fi
    
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

# Stop a running server (enhanced with verification)
stop_server() {
    local pid
    local status
    local stored_user stored_command
    
    pid=$(get_server_pid)
    status=$(check_server_status) || true
    
    if [[ "$status" == "stopped" ]]; then
        log_info "Server is not running."
        remove_pid_file
        return 0
    fi
    
    # Get stored verification info
    stored_user=$(get_pid_user)
    stored_command=$(get_pid_command)
    
    if [[ -n "$pid" ]] && is_process_running "$pid"; then
        # Verify this is our process before killing
        if ! verify_process "$pid" "$stored_user" "$stored_command"; then
            log_error "Process verification failed. Refusing to kill process $pid"
            log_error "This may be a stale PID file or the wrong process."
            log_error "Use --kill-port to forcefully kill by port, or manually verify."
            return 1
        fi
        
        log_info "Stopping server (PID: $pid)..."
        
        if [[ "$FORCE_STOP" == "true" ]]; then
            # Force kill mode - skip graceful shutdown
            log_info "Force killing server (--force specified)..."
            kill -9 "$pid" 2>/dev/null || true
        else
            # Graceful termination
            kill "$pid" 2>/dev/null || true
            # Wait for process to terminate
            local count=0
            while is_process_running "$pid" && [[ $count -lt "$GRACE_PERIOD" ]]; do
                sleep 1
                ((count++))
            done
            # Force kill if still running
            if is_process_running "$pid"; then
                log_info "Server did not terminate gracefully, force killing..."
                kill -9 "$pid" 2>/dev/null || true
            fi
        fi
    else
        log_warn "Process $pid not running, but port may still be in use"
        # Try to clean up port anyway
        local port=$(get_pid_port)
        if [[ -n "$port" ]] && is_port_in_use "$port"; then
            log_info "Attempting to free port $port..."
            kill_by_port "$port" "$FORCE_STOP"
        fi
    fi
    
    remove_pid_file
    log_info "Server stopped."
    return 0
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
    --force)
        FORCE_STOP=true
        shift
        ;;
    --kill-port)
        ACTION="kill-port"
        PORT="$2"
        shift 2
        ;;
    --restart)
        ACTION="restart"
        shift
        ;;
    --config)
        # Run the config generation script
        shift  # Remove --config from args
        CONFIG_SCRIPT="$PROOT/create_configs.sh"
        if [[ -f "$CONFIG_SCRIPT" ]]; then
            exec "$CONFIG_SCRIPT" "$@"
        else
            log_error "Config script not found: $CONFIG_SCRIPT"
            exit 1
        fi
        ;;
    --help | -h)
        cat << 'HELP_EOF'
Usage: start-mcp.sh [OPTIONS]

Start the CL-TRON-MCP server for MCP clients (OpenCode, Cursor, Kilocode).

Options:
  --use-sbcl       Use SBCL (error if not installed)
  --use-ecl        Use ECL (error if not installed)
  --stdio-only     Stdio only (short-lived, exits when client disconnects)
  --http-only      HTTP only (long-running)
  --port PORT      HTTP/WebSocket port (default: 4006)
  --websocket      Use WebSocket transport
  --status         Check if server is running
  --stop           Stop a running server gracefully
  --stop --force   Force kill a non-responsive server (skip graceful shutdown)
  --kill-port PORT Kill any process listening on specified port
  --restart        Stop existing server and start new one
  --restart --force Force stop before restart
  --config         Generate MCP client configuration files
  --help           Show this help

Transport Modes:
  --stdio-only     Short-lived process for MCP client communication
                   Process exits when the MCP client disconnects
  
  --http-only      Long-running HTTP server
                   Stays alive until stopped with Ctrl+C or --stop
  
  combined (default)  Long-running HTTP server (same as --http-only)
                   MCP clients should use streamable HTTP transport

Server Detection:
  For HTTP/combined modes, the script checks if a server is already running.
  If a healthy server is found, it exits successfully without starting a new one.
  
  PID file: .tron-server.pid (JSON format with pid, port, transport, user, command, unique_id)

Session Management:
  --status         Show server status (running/stopped, PID, port)
  --stop           Stop a running server gracefully (SIGTERM, then SIGKILL after 10s)
  --stop --force   Force kill immediately (SIGKILL)
  --kill-port PORT Kill any process using the specified port (bypasses PID file)
  --restart        Stop any existing server and start a new one
  --restart --force Force stop before restart

Examples:
  start-mcp.sh                      # Combined (long-running HTTP on 4006)
  start-mcp.sh --stdio-only         # Stdio only (for MCP client config)
  start-mcp.sh --http-only          # HTTP only on port 4006
  start-mcp.sh --http-only --port 9000
  start-mcp.sh --status             # Check server status
  start-mcp.sh --stop               # Stop running server gracefully
  start-mcp.sh --stop --force       # Force kill non-responsive server
  start-mcp.sh --kill-port 4006     # Kill whatever is on port 4006
  start-mcp.sh --restart            # Restart server
  start-mcp.sh --restart --force    # Force restart

Troubleshooting:
  - Server won't stop:     ./start-mcp.sh --stop --force
  - Port stuck:            ./start-mcp.sh --kill-port 4006
  - Stuck PID file:        ./start-mcp.sh --kill-port 4006 (then remove .tron-server.pid manually if needed)
  - Debug:                TRON_DEBUG=1 ./start-mcp.sh ...

MCP Client Config (Cursor ~/.cursor/mcp.json):
  {
    "mcpServers": {
      "tron": {
        "command": "~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh",
        "args": ["--stdio-only"]
      }
    }
  }

For long-running server (recommended for multiple sessions):
  1. Start manually: ./start-mcp.sh
  2. Configure client for streamable HTTP:
     "url": "http://127.0.0.1:4006/mcp"

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
    status=$(check_server_status) || true
    pid=$(get_server_pid)
    port=$(get_pid_port)
    transport=$(get_pid_transport)
    started=$(get_pid_started)
    user=$(get_pid_user)
    unique_id=$(get_pid_unique_id)
    
    # If no port from PID file, use http-port.txt or default
    if [[ -z "$port" ]]; then
        port=$(get_http_port)
    fi
    
    case "$status" in
    running)
        log_info "Server is RUNNING"
        log_info "  PID: $pid"
        log_info "  Port: $port"
        [[ -n "$transport" ]] && log_info "  Transport: $transport"
        [[ -n "$user" ]] && log_info "  User: $user"
        [[ -n "$unique_id" ]] && log_info "  Instance: $unique_id"
        [[ -n "$started" ]] && log_info "  Started: $(date -d @$started 2>/dev/null || date -r $started 2>/dev/null)"
        exit 0
        ;;
    running-external)
        log_info "Server is RUNNING (external instance)"
        log_info "  Port: $port"
        log_info "  Use --kill-port to stop it"
        exit 0
        ;;
    unhealthy)
        log_info "Server is UNHEALTHY"
        log_info "  PID: $pid"
        log_info "  Port: $port"
        [[ -n "$user" ]] && log_info "  User: $user"
        log_info "  Use --stop --force to force stop, or --restart"
        exit 1
        ;;
    port-in-use)
        log_info "Port $port is IN USE by another process"
        log_info "  Use --kill-port $port to stop it"
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

# Handle --kill-port action
if [[ "$ACTION" == "kill-port" ]]; then
    log_info "Killing process on port $PORT..."
    kill_by_port "$PORT" "$FORCE_STOP"
    exit $?
fi

# Handle --restart action
if [[ "$ACTION" == "restart" ]]; then
    status=$(check_server_status) || true
    if [[ "$status" == "running" ]] || [[ "$status" == "unhealthy" ]]; then
        stop_server
    elif [[ "$status" == "port-in-use" ]]; then
        log_info "Port in use but no PID file - attempting to free port..."
        kill_by_port "$(get_http_port)" "$FORCE_STOP"
    fi
    # Continue to start a new instance
fi

# For HTTP/combined modes, check if server is already running
if [[ "$TRANSPORT" != "stdio" ]]; then
    status=$(check_server_status) || true
    pid=$(get_server_pid)
    
    case "$status" in
    running)
        log_info "Server is already running (PID: $pid, Port: $(get_pid_port))"
        log_info "Use 'start-mcp.sh --stop' to stop it first."
        log_info "Use 'start-mcp.sh --restart' to stop and start a new instance."
        exit 0
        ;;
    running-external)
        log_info "Server is already running on port $(get_pid_port) (external instance)"
        exit 0
        ;;
    unhealthy)
        log_info "Server process exists but is unhealthy. Starting new instance..."
        stop_server
        ;;
    port-in-use)
        log_error "Port $(get_http_port) is in use by another process."
        log_error "Use '--port <different-port>' or '--kill-port <port>' to stop the conflicting process."
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
    log_info "  Mode: Long-running (use Ctrl+C or --stop to stop)"
else
    log_info "  Mode: Short-lived (exits when client disconnects)"
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

# Function to clean up PID file on exit (only for HTTP/combined modes)
cleanup() {
    local exit_code=$?
    if [[ "$TRANSPORT" != "stdio" ]] && [[ -f "$PID_FILE" ]]; then
        local pid_in_file=$(get_server_pid)
        if [[ "$pid_in_file" == "$$" ]]; then
            remove_pid_file
        fi
    fi
    exit $exit_code
}

# For HTTP/combined modes, set up PID file and cleanup trap
if [[ "$TRANSPORT" != "stdio" ]]; then
    trap cleanup EXIT
    write_pid_file "$$" "$PORT" "$TRANSPORT"
fi

# Build and execute the command
log_info ""
log_info "-------------------------------------------------------------------------"
log_info "--"
if [[ "$TRANSPORT" == "stdio" ]]; then
    log_info "-- Starting the stdio-only MCP"
    log_info "-- (Process will exit when client disconnects)"
    log_info "--"
    exec \
        "$LISP" \
        $LISP_QUIET \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(setq *compile-verbose* nil *load-verbose* nil)" \
        $LISP_EVAL "(push #p\"$PROOT/\" ql:*local-project-directories*)" \
        $LISP_EVAL "$COMPILE_EXPR" \
        $LISP_EVAL "$LOAD_EXPR" \
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :stdio-only) #+sbcl (sb-ext:exit :code 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
elif [[ "$TRANSPORT" == "combined" ]]; then
    # Combined mode: long-running HTTP server (stdio clients should use HTTP transport)
    log_info "-- Starting combined mode (long-running HTTP on port $PORT)"
    log_info "-- MCP clients can connect via streamable HTTP: http://127.0.0.1:$PORT/mcp"
    log_info "--"
    [[ "$LISP" = *sbcl* ]] && LISP_EXTRA="--non-interactive" || LISP_EXTRA=""
    # Write boot script then load it
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
#+sbcl (sb-ext:exit :code 0)
#+ecl (ext:quit 0)
#-(or sbcl ecl) (cl-user::quit 0)
BOOTEOF
    exec \
        "$LISP" \
        $LISP_QUIET \
        $LISP_EXTRA \
        "${ECL_ARGS[@]}" \
        $LISP_EVAL "(load #p\"$BOOT_FILE\")"
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
#+sbcl (sb-ext:exit :code 0)
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
        $LISP_EVAL "(progn (cl-tron-mcp/core:start-server :transport :websocket :port $PORT) #+sbcl (sb-ext:exit :code 0) #+ecl (ext:quit 0) #-(or sbcl ecl) (cl-user::quit))"
fi
