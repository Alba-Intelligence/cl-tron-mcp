# Tool Reference

## Tool Categories (86 tools total)

| Category   | Purpose                | Key Tools                                                      |
| ---------- | ---------------------- | -------------------------------------------------------------- |
| Inspector  | Object introspection   | `inspect_object`, `inspect_class`, `inspect_function`          |
| Debugger   | Debugging operations   | `debugger_frames`, `debugger_restarts`, `breakpoint_set`       |
| REPL       | Code evaluation        | `repl_eval`, `repl_frame_locals`                               |
| Hot Reload | Live code modification | `code_compile_string`, `reload_system`                         |
| Profiler   | Performance analysis   | `profile_start`, `profile_stop`                                |
| Tracer     | Function tracing       | `trace_function`, `trace_list`                                 |
| Threads    | Thread management      | `thread_list`, `thread_inspect`, `thread_backtrace`            |
| Monitor    | Production monitoring  | `health_check`, `runtime_stats`                                |
| Logging    | Package logging        | `log_configure`, `log_info`                                    |
| XRef       | Cross-reference        | `who_calls`, `who_references`, `list_callees`                  |
| Security   | Approval whitelist     | `whitelist_add`, `whitelist_status`                            |
| Swank      | Slime integration (21) | `swank_connect`, `swank_eval`, `swank_backtrace`, `swank_step` |
| Unified    | Swank REPL             | `repl_connect`, `repl_eval`, `repl_step`, `repl_continue`      |

## Inspector Tools (5)

- `inspect_object` - Inspect an object by ID
- `inspect_slot` - Get or set a slot value on an object
- `inspect_class` - Inspect a CLOS class definition
- `inspect_function` - Inspect a function definition
- `inspect_package` - Inspect a package and list its contents

## Debugger Tools (6)

- `debugger_frames` - Get debugger stack frames
- `debugger_restarts` - List available debugger restarts
- `breakpoint_set` - Set a breakpoint on a function (requires approval)
- `breakpoint_remove` - Remove a breakpoint by ID
- `breakpoint_list` - List all active breakpoints
- `step_frame` - Step execution in a frame

## REPL Tools (1)

- `repl_eval` - Evaluate Lisp code in REPL context (requires approval)

## Hot Reload Tools (2)

- `code_compile_string` - Compile and load Lisp code string (requires approval)
- `reload_system` - Reload ASDF system (requires approval)

## Profiler Tools (3)

- `profile_start` - Start deterministic profiling (requires approval)
- `profile_stop` - Stop profiling (requires approval)
- `profile_report` - Get profiling report

## Tracer Tools (3)

- `trace_function` - Add trace to a function (requires approval)
- `trace_remove` - Remove trace from a function (requires approval)
- `trace_list` - List all traced functions

## Thread Tools (3)

- `thread_list` - List all threads with their status
- `thread_inspect` - Get detailed information about a thread
- `thread_backtrace` - Get backtrace for a specific thread

## Monitor Tools (4)

- `health_check` - Basic health check for the MCP server
- `runtime_stats` - Get runtime statistics including memory and thread info
- `gc_run` - Force garbage collection
- `system_info` - Get comprehensive system information

## Logging Tools (5)

- `log_configure` - Configure logging level for a package
- `log_info` - Log an info message
- `log_debug` - Log a debug message
- `log_warn` - Log a warning message
- `log_error` - Log an error message

## Cross-Reference Tools (5)

- `who_calls` - Find functions that call a symbol
- `who_references` - Find references to a symbol
- `who_binds` - Find bindings of a symbol
- `who_sets` - Find setq/makunbound of a symbol
- `list_callees` - List functions called by a symbol

## Security/Approval Tools (5)

- `whitelist_add` - Add pattern to approval whitelist
- `whitelist_remove` - Remove pattern from approval whitelist
- `whitelist_clear` - Clear the approval whitelist
- `whitelist_enable` - Enable/disable the approval whitelist
- `whitelist_status` - Get current whitelist status

## Swank Tools (21) - Slime/Portacle Integration

**Connection:**
- `swank_connect` - Connect to Swank server
- `swank_disconnect` - Disconnect from Swank
- `swank_status` - Get Swank connection status

**Evaluation:**
- `swank_eval` - Evaluate Lisp code via Swank (requires approval)
- `swank_compile` - Compile Lisp code via Swank (requires approval)

**Threads:**
- `swank_threads` - List all threads in Swank-connected SBCL
- `swank_abort` - Abort a specific thread (requires approval)
- `swank_interrupt` - Interrupt evaluation (requires approval)

**Debugging:**
- `swank_backtrace` - Get the current backtrace
- `swank_get_restarts` - Get available restarts
- `swank_invoke_restart` - Invoke a restart by index
- `swank_continue` - Continue execution from debugger
- `swank_step` - Step into next expression
- `swank_next` - Step over next expression
- `swank_out` - Step out of current frame
- `swank_debugger_state` - Get debugger state

**Inspection:**
- `swank_inspect` - Inspect an object via Swank
- `swank_describe` - Describe a symbol via Swank
- `swank_autodoc` - Get documentation for a symbol
- `swank_completions` - Get symbol completions via Swank

## Unified REPL Tools (23) - Swank

**Connection:**
- `repl_connect` - Connect to Swank REPL
- `repl_disconnect` - Disconnect from the current REPL
- `repl_status` - Check REPL connection status and type

**Evaluation:**
- `repl_eval` - Evaluate Lisp code via the connected REPL (requires approval)
- `repl_compile` - Compile Lisp code via the connected REPL (requires approval)

**Debugging:**
- `repl_backtrace` - Get backtrace via the connected REPL
- `repl_frame_locals` - Get local variables for a frame
- `repl_get_restarts` - Get available restarts
- `repl_invoke_restart` - Invoke a restart by index
- `repl_step` - Step into next expression
- `repl_next` - Step over next expression
- `repl_out` - Step out of current frame
- `repl_continue` - Continue execution from debugger

**Breakpoints:**
- `repl_set_breakpoint` - Set a breakpoint on a function (requires approval)
- `repl_remove_breakpoint` - Remove a breakpoint by ID
- `repl_list_breakpoints` - List all breakpoints
- `repl_toggle_breakpoint` - Toggle breakpoint enabled state

**Inspection:**
- `repl_inspect` - Inspect an object via the connected REPL
- `repl_describe` - Describe a symbol via the connected REPL
- `repl_completions` - Get symbol completions via the connected REPL
- `repl_doc` - Get documentation for a symbol via the connected REPL

**Threads:**
- `repl_threads` - List all threads via the connected REPL
- `repl_abort` - Abort/interrupt evaluation via the connected REPL

**Help:**
- `repl_help` - Get help on available unified REPL tools