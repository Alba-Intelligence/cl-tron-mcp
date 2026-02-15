#!/usr/bin/env bash
# Generate demo GIF using VHS
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
	sleep 2
fi

echo "VHS version: $(vhs --version)"
echo "Temp directory: $TMPDIR"
echo ""
echo "Generating demo.gif..."
echo "  - Starts Swank server"
echo "  - Connects with Tron"
echo "  - Runs factorial debugging demo"
echo ""

vhs demo.tape

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  ✅ Done!"
echo "═══════════════════════════════════════════════════════════════════════════════"
ls -la demo.gif
echo ""
echo "Use in README.md:"
echo "  ![Tron Demo](demo/demo.gif)"

# Cleanup temp directory
rm -rf "$TMPDIR"
