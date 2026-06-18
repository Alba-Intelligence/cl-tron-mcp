#!/usr/bin/env bash
# run-mcp.sh - Start CL-TRON-MCP from outside the devenv environment
#
# Enters the devenv environment with QUICKLISP_DIR auto-detected from the
# host, then runs start-mcp.sh with all arguments passed through.
#
# Quicklisp lookup order:
#   1. QUICKLISP_DIR env var (if set)
#   2. $HOME/quicklisp
#   3. Other common user homes on the same system

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v devenv &>/dev/null; then
    echo "ERROR: devenv is not installed or not in PATH" >&2
    echo "Install devenv: https://devenv.sh/install/" >&2
    exit 1
fi

HOST_HOME="${HOME:-}"
if [[ -z "$HOST_HOME" ]]; then
    HOST_HOME="$(eval echo ~$(whoami) 2>/dev/null || true)"
fi

if [[ -z "${QUICKLISP_DIR:-}" ]]; then
    if [[ -n "$HOST_HOME" && -f "$HOST_HOME/quicklisp/setup.lisp" ]]; then
        export QUICKLISP_DIR="$HOST_HOME/quicklisp"
    else
        echo "ERROR: Could not find Quicklisp setup.lisp" >&2
        echo "Install Quicklisp (https://www.quicklisp.org/beta/) or set QUICKLISP_DIR." >&2
        exit 1
    fi
fi

exec env QUICKLISP_DIR="$QUICKLISP_DIR" devenv shell -- "$SCRIPT_DIR/start-mcp.sh" "$@"
