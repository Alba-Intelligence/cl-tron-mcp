#!/usr/bin/env bash
# demo/record.sh — Record Tron MCP demos with asciinema + agg
#
# Prerequisites (in devenv.nix): asciinema_3, asciinema-agg
#
# Usage:
#   ./demo/record.sh                  # Record all scenarios
#   ./demo/record.sh mcp-overview     # Record one scenario
#   ./demo/record.sh --cast-only      # Record .cast files only, skip GIF conversion
#
# Output:
#   demo/<scenario>.cast   — asciinema recording (inspectable text)
#   demo/<scenario>.gif    — animated GIF for README embedding

set -e

cd "$(dirname "$0")/.."

SCENARIOS=("mcp-overview" "f1-f2" "factorial")
CAST_ONLY=false
SELECTED=()

for arg in "$@"; do
    case "$arg" in
        --cast-only) CAST_ONLY=true ;;
        mcp-overview|f1-f2|factorial) SELECTED+=("$arg") ;;
        *) echo "Unknown argument: $arg"; echo "Usage: $0 [scenario...] [--cast-only]"; exit 1 ;;
    esac
done

[ ${#SELECTED[@]} -eq 0 ] && SELECTED=("${SCENARIOS[@]}")

echo "═══════════════════════════════════════════════════════════════════════════"
echo "  Tron MCP Demo Recorder"
echo "  Scenarios: ${SELECTED[*]}"
echo "═══════════════════════════════════════════════════════════════════════════"
echo ""

# Check required tools
for tool in asciinema agg python3; do
    command -v "$tool" &>/dev/null || { echo "Error: $tool not found"; exit 1; }
done
echo "asciinema: $(asciinema --version 2>&1)"
echo "agg:       $(agg --version 2>&1 | head -1)"
echo ""

# Kill any orphan Swank on port 14006
if ss -tlnp 2>/dev/null | grep -q 14006; then
    echo "Warning: port 14006 in use, killing orphan process..."
    pid=$(ss -tlnp 2>/dev/null | grep 14006 | grep -oP '(?<=pid=)\d+' | head -1)
    [ -n "$pid" ] && kill "$pid" 2>/dev/null && sleep 1
fi

for scenario in "${SELECTED[@]}"; do
    echo "───────────────────────────────────────────────────────────────────────────"
    echo "  Recording: $scenario"
    echo "───────────────────────────────────────────────────────────────────────────"

    cast_file="demo/${scenario}.cast"
    gif_file="demo/${scenario}.gif"

    # Record with asciinema
    echo "  → asciinema rec $cast_file ..."
    asciinema rec "$cast_file" \
        --overwrite \
        --cols 110 \
        --rows 38 \
        --command "python3 demo/run-demo.py $scenario"
    echo "  ✓ Cast saved: $cast_file ($(du -h "$cast_file" | cut -f1))"

    if [ "$CAST_ONLY" = false ]; then
        # Convert to GIF with agg
        echo "  → agg $cast_file $gif_file ..."
        agg "$cast_file" "$gif_file" \
            --font-family "JetBrains Mono,Consolas,monospace" \
            --font-size 14 \
            --speed 1.0
        echo "  ✓ GIF saved:  $gif_file ($(du -h "$gif_file" | cut -f1))"
    fi

    echo ""
done

echo "═══════════════════════════════════════════════════════════════════════════"
echo "  ✅ Done!"
echo "═══════════════════════════════════════════════════════════════════════════"
echo ""
echo "Files:"
ls -lh demo/*.cast demo/*.gif 2>/dev/null || true
echo ""
if [ "$CAST_ONLY" = false ]; then
    echo "Embed in README.md:"
    for scenario in "${SELECTED[@]}"; do
        echo "  ![${scenario} demo](demo/${scenario}.gif)"
    done
fi
