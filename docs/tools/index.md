# Tool Reference

This directory contains the per-tool documentation for the MCP surface exposed by `cl-tron-mcp`.

## How to Read the Tool Surface

- **Use `repl_*` first** for normal agent workflows. These are the higher-level, unified tools built on top of Swank.
- **Use `swank_*`** when you need lower-level control or want behavior that maps closely to raw Swank operations.
- **Use the managed-process tools** when Tron itself should launch a fresh SBCL+Swank child process.
- **Use inspector / debugger / xref / monitor tools** when no code execution is needed.

The current registry contains **91 tools** across **14 categories**.

## Categories

### Unified REPL Tools (24)

Preferred high-level interface for live Lisp workflows:

- [repl-connect](repl-connect.md)
- [repl-disconnect](repl-disconnect.md)
- [repl-status](repl-status.md)
- [repl-eval](repl-eval.md)
- [repl-compile](repl-compile.md)
- [repl-threads](repl-threads.md)
- [repl-abort](repl-abort.md)
- [repl-backtrace](repl-backtrace.md)
- [repl-inspect](repl-inspect.md)
- [repl-describe](repl-describe.md)
- [repl-completions](repl-completions.md)
- [repl-doc](repl-doc.md)
- [repl-frame-locals](repl-frame-locals.md)
- [repl-step](repl-step.md)
- [repl-next](repl-next.md)
- [repl-out](repl-out.md)
- [repl-continue](repl-continue.md)
- [repl-get-restarts](repl-get-restarts.md)
- [repl-invoke-restart](repl-invoke-restart.md)
- [repl-set-breakpoint](repl-set-breakpoint.md)
- [repl-remove-breakpoint](repl-remove-breakpoint.md)
- [repl-list-breakpoints](repl-list-breakpoints.md)
- [repl-toggle-breakpoint](repl-toggle-breakpoint.md)
- [repl-help](repl-help.md)

### Swank Tools (21)

Lower-level Swank-oriented operations:

- [swank-connect](swank-connect.md)
- [swank-disconnect](swank-disconnect.md)
- [swank-status](swank-status.md)
- [swank-eval](swank-eval.md)
- [swank-compile](swank-compile.md)
- [swank-threads](swank-threads.md)
- [swank-abort](swank-abort.md)
- [swank-interrupt](swank-interrupt.md)
- [swank-backtrace](swank-backtrace.md)
- [swank-inspect](swank-inspect.md)
- [swank-describe](swank-describe.md)
- [swank-autodoc](swank-autodoc.md)
- [swank-completions](swank-completions.md)
- [swank-get-restarts](swank-get-restarts.md)
- [swank-invoke-restart](swank-invoke-restart.md)
- [swank-continue](swank-continue.md)
- [swank-step](swank-step.md)
- [swank-next](swank-next.md)
- [swank-out](swank-out.md)
- [swank-debugger-state](swank-debugger-state.md)
- [swank-send-input](swank-send-input.md)

### Managed Swank Process Tools (4)

Tools for launching and supervising SBCL+Swank child processes from Tron:

- [swank-launch](swank-launch.md)
- [swank-kill](swank-kill.md)
- [swank-process-list](swank-process-list.md)
- [swank-process-status](swank-process-status.md)

### Debugger Tools (7)

- [debugger-frames](debugger-frames.md)
- [debugger-restarts](debugger-restarts.md)
- [breakpoint-set](breakpoint-set.md)
- [breakpoint-remove](breakpoint-remove.md)
- [breakpoint-list](breakpoint-list.md)
- [breakpoint-toggle](breakpoint-toggle.md)
- [step-frame](step-frame.md)

### Inspector Tools (5)

- [inspect-object](inspect-object.md)
- [inspect-slot](inspect-slot.md)
- [inspect-class](inspect-class.md)
- [inspect-function](inspect-function.md)
- [inspect-package](inspect-package.md)

### Hot Reload Tools (2)

- [code-compile-string](code-compile-string.md)
- [reload-system](reload-system.md)

### Profiler Tools (3)

- [profile-start](profile-start.md)
- [profile-stop](profile-stop.md)
- [profile-report](profile-report.md)

### Tracer Tools (3)

- [trace-function](trace-function.md)
- [trace-remove](trace-remove.md)
- [trace-list](trace-list.md)

### Thread Tools (3)

- [thread-list](thread-list.md)
- [thread-inspect](thread-inspect.md)
- [thread-backtrace](thread-backtrace.md)

### Monitor Tools (4)

- [health-check](health-check.md)
- [runtime-stats](runtime-stats.md)
- [gc-run](gc-run.md)
- [system-info](system-info.md)

### Logging Tools (5)

- [log-configure](log-configure.md)
- [log-info](log-info.md)
- [log-debug](log-debug.md)
- [log-warn](log-warn.md)
- [log-error](log-error.md)

### XRef Tools (5)

- [who-calls](who-calls.md)
- [who-references](who-references.md)
- [who-binds](who-binds.md)
- [who-sets](who-sets.md)
- [list-callees](list-callees.md)

### Security Tools (5)

- [whitelist-add](whitelist-add.md)
- [whitelist-remove](whitelist-remove.md)
- [whitelist-clear](whitelist-clear.md)
- [whitelist-enable](whitelist-enable.md)
- [whitelist-status](whitelist-status.md)

### Legacy REPL Tool (1)

- [repl-eval-repl](repl-eval-repl.md)

## Related References

- [Code Reference](../code-reference.md) - source-level subsystem map
- [Architecture](../architecture.md) - process model and runtime flow
- [Starting the MCP Server](../starting-the-mcp.md) - installation, startup, and verification
