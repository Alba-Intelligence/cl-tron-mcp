# Tool Documentation Index

This directory contains detailed documentation for all MCP tools provided by cl-tron-mcp.

## Tool Categories

### Unified REPL Tools (24 tools)
- [repl-connect](repl-connect.md) - Connect to Swank REPL
- [repl-disconnect](repl-disconnect.md) - Disconnect from REPL
- [repl-status](repl-status.md) - Check REPL connection status
- [repl-eval](repl-eval.md) - Evaluate Lisp code in REPL
- [repl-compile](repl-compile.md) - Compile and load Lisp code
- [repl-threads](repl-threads.md) - List all REPL threads
- [repl-abort](repl-abort.md) - Abort/interrupt REPL evaluation
- [repl-backtrace](repl-backtrace.md) - Get REPL call stack
- [repl-inspect](repl-inspect.md) - Inspect object in REPL
- [repl-describe](repl-describe.md) - Describe symbol in REPL
- [repl-completions](repl-completions.md) - Get symbol completions
- [repl-doc](repl-doc.md) - Get symbol documentation
- [repl-frame-locals](repl-frame-locals.md) - Get frame local variables
- [repl-step](repl-step.md) - Step into next expression
- [repl-next](repl-next.md) - Step over next expression
- [repl-out](repl-out.md) - Step out of current frame
- [repl-continue](repl-continue.md) - Continue from debugger
- [repl-get-restarts](repl-get-restarts.md) - Get available restarts
- [repl-invoke-restart](repl-invoke-restart.md) - Invoke a restart
- [repl-set-breakpoint](repl-set-breakpoint.md) - Set a breakpoint
- [repl-remove-breakpoint](repl-remove-breakpoint.md) - Remove a breakpoint
- [repl-list-breakpoints](repl-list-breakpoints.md) - List all breakpoints
- [repl-toggle-breakpoint](repl-toggle-breakpoint.md) - Toggle breakpoint state
- [repl-help](repl-help.md) - Get help on REPL tools

### Swank Tools (20 tools)
- [swank-connect](swank-connect.md) - Connect to Swank server
- [swank-disconnect](swank-disconnect.md) - Disconnect from Swank
- [swank-status](swank-status.md) - Get Swank connection status
- [swank-eval](swank-eval.md) - Evaluate code in SBCL
- [swank-compile](swank-compile.md) - Compile and load code
- [swank-threads](swank-threads.md) - List all SBCL threads
- [swank-abort](swank-abort.md) - Abort a thread
- [swank-interrupt](swank-interrupt.md) - Interrupt current thread
- [swank-backtrace](swank-backtrace.md) - Get call stack
- [swank-inspect](swank-inspect.md) - Inspect object in SBCL
- [swank-describe](swank-describe.md) - Describe symbol in SBCL
- [swank-autodoc](swank-autodoc.md) - Get argument list and docs
- [swank-completions](swank-completions.md) - Get symbol completions
- [swank-get-restarts](swank-get-restarts.md) - Get available restarts
- [swank-invoke-restart](swank-invoke-restart.md) - Invoke a restart
- [swank-continue](swank-continue.md) - Continue from debugger
- [swank-step](swank-step.md) - Step into next expression
- [swank-next](swank-next.md) - Step over next expression
- [swank-out](swank-out.md) - Step out of current frame
- [swank-debugger-state](swank-debugger-state.md) - Get debugger state

### Inspector Tools (5 tools)
- [inspect-object](inspect-object.md) - Inspect object by ID
- [inspect-slot](inspect-slot.md) - Get or set slot value
- [inspect-class](inspect-class.md) - Inspect CLOS class
- [inspect-function](inspect-function.md) - Inspect function definition
- [inspect-package](inspect-package.md) - Inspect package symbols

### Debugger Tools (6 tools)
- [debugger-frames](debugger-frames.md) - Get debugger stack frames
- [debugger-restarts](debugger-restarts.md) - List debugger restarts
- [breakpoint-set](breakpoint-set.md) - Set a breakpoint
- [breakpoint-remove](breakpoint-remove.md) - Remove a breakpoint
- [breakpoint-list](breakpoint-list.md) - List all breakpoints
- [step-frame](step-frame.md) - Step execution in frame

### REPL Tools (1 tool)
- [repl-eval-repl](repl-eval-repl.md) - Evaluate code in REPL

### Hot Reload Tools (2 tools)
- [code-compile-string](code-compile-string.md) - Compile and load code string
- [reload-system](reload-system.md) - Reload ASDF system

### Profiler Tools (3 tools)
- [profile-start](profile-start.md) - Start profiling
- [profile-stop](profile-stop.md) - Stop profiling
- [profile-report](profile-report.md) - Get profiling report

### Tracer Tools (3 tools)
- [trace-function](trace-function.md) - Trace a function
- [trace-remove](trace-remove.md) - Remove function trace
- [trace-list](trace-list.md) - List traced functions

### Thread Tools (3 tools)
- [thread-list](thread-list.md) - List all threads
- [thread-inspect](thread-inspect.md) - Inspect thread details
- [thread-backtrace](thread-backtrace.md) - Get thread backtrace

### Monitor Tools (4 tools)
- [health-check](health-check.md) - Server health check
- [runtime-stats](runtime-stats.md) - Get runtime statistics
- [gc-run](gc-run.md) - Force garbage collection
- [system-info](system-info.md) - Get system information

### Logging Tools (5 tools)
- [log-configure](log-configure.md) - Configure logging level
- [log-info](log-info.md) - Log info message
- [log-debug](log-debug.md) - Log debug message
- [log-warn](log-warn.md) - Log warning message
- [log-error](log-error.md) - Log error message

### XRef Tools (5 tools)
- [who-calls](who-calls.md) - Find function callers
- [who-references](who-references.md) - Find symbol references
- [who-binds](who-binds.md) - Find symbol bindings
- [who-sets](who-sets.md) - Find symbol modifications
- [list-callees](list-callees.md) - List called functions

### Security Tools (5 tools)
- [whitelist-add](whitelist-add.md) - Add whitelist pattern
- [whitelist-remove](whitelist-remove.md) - Remove whitelist pattern
- [whitelist-clear](whitelist-clear.md) - Clear whitelist
- [whitelist-enable](whitelist-enable.md) - Enable/disable whitelist
- [whitelist-status](whitelist-status.md) - Get whitelist status

## Total Tools: 86

## Documentation Format

Each tool documentation file follows this structure:

```markdown
# Tool Name

**Short Description:** Brief one-line description

**Full Description:** [The verbose description from the original code]

**Parameters:**
- `param1`: Description
- `param2`: Description

**Returns:** Description of return value

**Example Usage:**
```lisp
(tool-name :param1 value1 :param2 value2)
```

**Notes:** Any additional notes, prerequisites, or warnings
```

## Accessing Documentation

Documentation can be accessed via:
1. MCP `resources/list` - Lists all documentation files
2. MCP `resources/read` - Reads a specific documentation file
3. Tool descriptors include `documentationUri` field pointing to the documentation