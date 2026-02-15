#!/usr/bin/env bash
# Generate demo GIFs using VHS
#
# Prerequisites:
#   - VHS installed: brew install vhs
#   - Quicklisp available
#   - No Swank server running on port 4005

set -e

cd "$(dirname "$0")"

# Use local tmp directory instead of /tmp
export TMPDIR="$(pwd)/tmp"
mkdir -p "$TMPDIR"

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  Tron Demo Generator"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""

# Check VHS is installed
if ! command -v vhs &>/dev/null; then
	echo "Error: VHS is not installed."
	echo "Install with: brew install vhs"
	echo "Or: go install github.com/charmbracelet/vhs@latest"
	exit 1
fi

# Check port 4005 is free
if ss -tlnp 2>/dev/null | grep -q 4005; then
	echo "Warning: Port 4005 is in use. Killing existing Swank..."
	pkill -f 'swank:create-server' || true
	pkill -f 'start-swank' || true
	sleep 2
fi

echo "VHS version: $(vhs --version)"
echo "Temp directory: $TMPDIR"
echo ""

# Generate all demos
for tape in demo.tape demo-kilocode.tape demo-mcp-raw.tape; do
	gif="${tape%.tape}.gif"
	echo "Generating $tape → $gif..."
	vhs "$tape" 2>&1 | grep -E "(Creating|Error|failed)" || true
	echo ""
done

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  ✅ Done!"
echo "═══════════════════════════════════════════════════════════════════════════════"
ls -la *.gif
echo ""
echo "GIFs generated:"
echo "  demo.gif          - Basic debugging demo"
echo "  demo-kilocode.gif - Kilocode CLI simulation"
echo "  demo-mcp-raw.gif  - Raw MCP protocol messages"
echo ""
echo "Use in README.md:"
echo "  ![Tron Demo](demo/demo.gif)"

# Cleanup temp directory
rm -rf "$TMPDIR"
