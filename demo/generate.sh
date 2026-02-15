#!/bin/bash
# Generate all demo GIFs using VHS

set -e

cd "$(dirname "$0")"

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

echo "VHS version: $(vhs --version)"
echo ""

# Generate individual GIFs
echo "Generating demo GIFs..."
echo ""

for tape in *.tape; do
	gif="${tape%.tape}.gif"
	echo "  📹 $tape → $gif"
	vhs "$tape" 2>/dev/null
done

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  ✅ Done! Generated GIFs:"
echo "═══════════════════════════════════════════════════════════════════════════════"
ls -la *.gif
echo ""
echo "Use in README.md:"
echo "  ![Connect](demo/01-connect.gif)"
echo "  ![Error](demo/02-error.gif)"
echo "  ![Inspect](demo/03-inspect.gif)"
echo "  ![Fix](demo/04-fix.gif)"
echo "  ![Summary](demo/05-summary.gif)"
echo ""
echo "Or the full demo:"
echo "  ![Factorial Demo](demo/factorial-demo.gif)"
