# Tron MCP Demos

Animated demos showing Tron's actual **MCP JSON-RPC protocol** — the real messages that AI agents send to the server.

Each demo is split into focused phases so you can see exactly what's happening at each stage.

---

## Demo 1: Protocol Discovery

> An AI agent starts by discovering what Tron offers: a catalog of tools, documentation resources, and guided workflow prompts.

### Part 1: Discovering Tools, Resources, and Prompts

Tron exposes 86 tools, documentation files as MCP resources, and 9 guided workflow prompts.
The agent uses standard MCP discovery (`tools/list`, `resources/list`, `prompts/list`) to learn what's available.

![Protocol discovery](mcp-overview-1.gif)

### Part 2: Health Check and Runtime Statistics

Once connected to a live Lisp image, Tron can report health status and detailed runtime statistics.

![Health and stats](mcp-overview-2.gif)

---

## Demo 2: f1/f2 — Live In-Debugger Hot-Reload

> This demo showcases one of Common Lisp's most powerful features: the ability to compile and load new code **while the debugger is active**, then retry the failing call without unwinding the call stack.

### Part 1: Connecting to a Live SBCL Image

We start the MCP server, launch a fresh SBCL process with Swank/SLIME, and confirm the REPL is live.

![Setup and connection](f1-f2-1.gif)

### Part 2: The In-Debugger Fix

We compile `f1` which calls the undefined `f2`, triggering `UNDEFINED-FUNCTION`.
Instead of aborting, we compile `f2` **while still in the debugger**, then invoke the `CONTINUE` restart to retry the
original call — which now succeeds. The call stack was never unwound.

![In-debugger hot-reload](f1-f2-2.gif)

---

## Demo 3: Factorial — Debugging a Runtime Error

> A buggy `factorial` implementation uses `(/ 1 0)` as its base case. We trigger the error, inspect the debugger state,
> hot-load a corrected implementation, and verify the fix.

### Part 1: Setting Up the Bug

We connect to a fresh Lisp image, compile the buggy `factorial`, and trigger `DIVISION-BY-ZERO`.

![Setup with bug](factorial-1.gif)

### Part 2: Debugging, Fixing, and Verifying

We inspect the restarts, abort the debugger, hot-reload the corrected `factorial`,
and verify `(factorial 5) = 120`, `(factorial 7) = 5040`, `(factorial 10) = 3628800` — all without restarting SBCL.

![Debug, fix, verify](factorial-2.gif)

---

## How It Works

Each demo runs `demo/run-demo.py <scenario>` which:
1. Starts `./start-mcp.sh --stdio-only` as a subprocess
2. Sends real JSON-RPC requests to its stdin
3. Reads JSON-RPC responses from its stdout
4. Prints formatted `→ request` / `← response` pairs

This is exactly what Claude Code, Cursor, Kilocode, and OpenCode do when they use Tron.

## Running Demos

```bash
# Run a phase scenario manually (no recording)
python3 demo/run-demo.py mcp-overview-1
python3 demo/run-demo.py f1-f2-2
python3 demo/run-demo.py factorial-2

# Re-record all demos (requires asciinema + agg in devenv.nix)
./demo/record.sh

# Record a single scenario
./demo/record.sh f1-f2-2

# Record .cast files only (skip GIF conversion)
./demo/record.sh --cast-only
```

## Files

| File | Purpose |
|------|---------|
| `run-demo.py` | Python demo driver (6 phase scenarios + 3 legacy full scenarios) |
| `record.sh` | asciinema recording + agg GIF generation |
| `mcp-overview-1.cast` / `.gif` | Protocol discovery (tools, resources, prompts) |
| `mcp-overview-2.cast` / `.gif` | Health check and runtime stats |
| `f1-f2-1.cast` / `.gif` | Swank launch and REPL connection |
| `f1-f2-2.cast` / `.gif` | In-debugger hot-reload (the highlight demo) |
| `factorial-1.cast` / `.gif` | Buggy factorial setup |
| `factorial-2.cast` / `.gif` | Debug, fix, and verify |


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

