#!/usr/bin/env bash
# Capture one MCP request/response over stdio for debugging Kilocode (or any client).
# Writes: reports/mcp-stdio-out.log (first line from server), reports/mcp-stdio-err.log (stderr).
# Usage: from project root, ./scripts/debug-mcp-stdio.sh [path-to-start-mcp.sh]

set -e
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"
START_SCRIPT="${1:-$PROJECT_ROOT/start-mcp.sh}"
REPORTS="$PROJECT_ROOT/reports"
mkdir -p "$REPORTS"

INIT_LINE='{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'

echo "Sending one initialize, capturing first stdout line and stderr..."
# Allow up to 90s for first run (compile); precompile first for faster debug
echo "$INIT_LINE" | timeout 90 "$START_SCRIPT" --stdio-only 2>"$REPORTS/mcp-stdio-err.log" | head -1 >"$REPORTS/mcp-stdio-out.log" || true

if [[ -s "$REPORTS/mcp-stdio-out.log" ]]; then
  first=$(head -c 1 "$REPORTS/mcp-stdio-out.log")
  if [[ "$first" == "{" ]]; then
    echo "OK: First line is JSON object (starts with {). See $REPORTS/mcp-stdio-out.log"
  else
    echo "FAIL: First character is '$first' (expected '{'). See $REPORTS/mcp-stdio-out.log and $REPORTS/mcp-stdio-err.log"
  fi
else
  echo "FAIL: No stdout. Check $REPORTS/mcp-stdio-err.log"
fi
