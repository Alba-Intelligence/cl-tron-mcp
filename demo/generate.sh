#!/usr/bin/env bash
# Generate demo GIFs using VHS
#
# Prerequisites:
#   - VHS available (NixOS: included via devenv; others: go install github.com/charmbracelet/vhs@latest)
#   - Quicklisp available
#   - LD_LIBRARY_PATH set for OpenSSL if on NixOS

set -e

cd "$(dirname "$0")"

# Use local tmp directory instead of /tmp
export TMPDIR="$(pwd)/tmp"
mkdir -p "$TMPDIR"

# NixOS: ensure libcrypto is available for CFFI
if [ -d /nix/store ]; then
  OPENSSL_LIB=$(find /nix/store -maxdepth 1 -name '*openssl-3*' -type d 2>/dev/null | head -1)
  if [ -n "$OPENSSL_LIB" ] && [ -d "$OPENSSL_LIB/lib" ]; then
    export LD_LIBRARY_PATH="${OPENSSL_LIB}/lib:${LD_LIBRARY_PATH:-}"
  fi
fi

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  Tron Demo Generator"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""

# Find VHS
VHS_BIN=""
if command -v vhs &>/dev/null; then
	VHS_BIN="vhs"
elif [ -n "$(find /nix/store -maxdepth 2 -name 'vhs' -type f 2>/dev/null | head -1)" ]; then
	VHS_BIN="$(find /nix/store -maxdepth 2 -name 'vhs' -type f 2>/dev/null | head -1)"
fi

if [ -z "$VHS_BIN" ]; then
	echo "Error: VHS is not installed."
	echo "Install with: go install github.com/charmbracelet/vhs@latest"
	exit 1
fi

echo "VHS: $VHS_BIN"
echo "Temp directory: $TMPDIR"
echo "LD_LIBRARY_PATH: ${LD_LIBRARY_PATH:-<not set>}"
echo ""

# Generate all demos
for tape in demo.tape demo-kilocode.tape demo-mcp-raw.tape demo-f1-f2.tape; do
	if [ ! -f "$tape" ]; then
		echo "Skipping $tape (not found)"
		continue
	fi
	gif="${tape%.tape}.gif"
	echo "Generating $tape → $gif..."
	"$VHS_BIN" "$tape" 2>&1 | grep -E "(Creating|Error|failed)" || true
	echo ""
done

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  ✅ Done!"
echo "═══════════════════════════════════════════════════════════════════════════════"
ls -la *.gif 2>/dev/null || echo "(no GIFs generated)"
echo ""
echo "GIFs generated:"
echo "  demo.gif          - Basic debugging demo"
echo "  demo-kilocode.gif - Kilocode CLI simulation"
echo "  demo-mcp-raw.gif  - Raw MCP protocol messages"
echo "  demo-f1-f2.gif    - f1/f2 hot-reload demo"
echo ""
echo "Use in README.md:"
echo "  ![Tron Demo](demo/demo.gif)"

# Cleanup temp directory
rm -rf "$TMPDIR"
