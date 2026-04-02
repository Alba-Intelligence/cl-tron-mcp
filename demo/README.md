# Tron MCP Demos

Animated demos showing Tron's actual **MCP JSON-RPC protocol** — the real messages that AI agents send to the server.

## Demos

### 1. Protocol Overview (`mcp-overview`)

Discover Tron: 91 tools across 19 categories, 20 documentation resources, 9 guided workflow prompts, plus health check and runtime stats.

![Protocol Overview](mcp-overview.gif)

### 2. f1/f2 Hot-Reload (`f1-f2`)

The canonical hot-reload workflow: define `f1` calling undefined `f2`, trigger `UNDEFINED-FUNCTION`, hot-compile `f2` into the live image, verify.

![f1/f2 Hot-Reload](f1-f2.gif)

### 3. Factorial Debugging (`factorial`)

A buggy `factorial` triggers `DIVISION-BY-ZERO`. Inspect the debugger, hot-reload the fix, verify `(factorial 5) = 120`, `(factorial 10) = 3628800`.

![Factorial Debug](factorial.gif)

## How It Works

Each demo runs `demo/run-demo.py <scenario>` which:
1. Starts `./start-mcp.sh --stdio-only` as a subprocess
2. Sends real JSON-RPC requests to its stdin
3. Reads JSON-RPC responses from its stdout
4. Prints formatted `→ request` / `← response` pairs

This is exactly what Claude Code, Cursor, Kilocode, and OpenCode do when they use Tron.

## Running Demos

```bash
# Run a demo scenario manually (no recording)
python3 demo/run-demo.py mcp-overview
python3 demo/run-demo.py f1-f2
python3 demo/run-demo.py factorial

# Re-record all demos (requires asciinema + agg in devenv.nix)
./demo/record.sh

# Record a single scenario
./demo/record.sh f1-f2

# Record .cast files only (skip GIF conversion)
./demo/record.sh --cast-only
```

## Files

| File | Purpose |
|------|---------|
| `run-demo.py` | Python demo driver (3 scenarios) |
| `record.sh` | asciinema recording + agg GIF generation |
| `mcp-overview.cast` | asciinema recording (protocol overview) |
| `f1-f2.cast` | asciinema recording (hot-reload demo) |
| `factorial.cast` | asciinema recording (debugging demo) |
| `mcp-overview.gif` | Animated GIF (protocol overview) |
| `f1-f2.gif` | Animated GIF (hot-reload demo) |
| `factorial.gif` | Animated GIF (debugging demo) |

