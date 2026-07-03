# Tool Reference

Use this page when you need a fast mental model of the MCP surface before drilling into the full per-tool docs.

## Current Inventory

Tron currently exposes **91 tools** across **14 categories**.

## Which Layer to Prefer

1. **`repl_*` tools** - preferred high-level workflow for most agents
2. **`swank_*` tools** - lower-level operations close to raw Swank behavior
3. **managed-process tools** - launch or supervise SBCL+Swank child processes
4. **inspector / debugger / xref / monitor tools** - read-heavy operations outside the compile/eval flow

## Categories

| Category | Count | Typical use |
| --- | ---: | --- |
| Unified REPL | 24 | Connect, evaluate, compile, inspect, debug, continue |
| Swank | 21 | Raw Swank-oriented control and debugger interaction |
| Managed Processes | 4 | Launch/list/status/kill SBCL+Swank children |
| Debugger | 7 | Frames, restarts, breakpoints, frame stepping |
| Inspector | 5 | Objects, slots, classes, functions, packages |
| Hot Reload | 2 | Compile code strings and reload ASDF systems |
| Profiler | 3 | Start, stop, and report profiling |
| Tracer | 3 | Add/remove/list traces |
| Threads | 3 | Thread list, inspect, backtrace |
| Monitor | 4 | Health, runtime stats, GC, system info |
| Logging | 5 | Configure logging and emit log messages |
| XRef | 5 | `who_*` and callee lookup |
| Security | 5 | Approval whitelist management |
| Legacy REPL | 1 | Historical direct REPL entrypoint |

## Fast Recommendations

- Need to start working with a live Lisp image? Use `repl_connect`.
- Need to evaluate or patch code? Use `repl_eval` / `repl_compile`.
- Need stack and restart information? Use `repl_backtrace`, `repl_frame_locals`, `repl_get_restarts`, `repl_invoke_restart`.
- Need lower-level control or special Swank behavior? Drop to the matching `swank_*` tool.
- Need an ephemeral child Lisp session? Use `swank_launch`.

## Full Reference

Use [../tools/index.md](../tools/index.md) for the complete per-tool catalog and [../code-reference.md](../code-reference.md) for the corresponding source files.
