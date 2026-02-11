# SBCL Debugging Workflows

This guide provides workflows for debugging Common Lisp applications using the SBCL Debugging MCP tools.

## Quick Reference

**Debugging Tool Cheat Sheet:**
| Task | Tool | Key Parameters |
|------|------|----------------|
| Get stack frames | `debugger_frames` | thread, start, end |
| Inspect locals | `debugger_frame_locals` | thread, frame |
| Evaluate in frame | `debugger_eval_in_frame` | thread, frame, code |
| List restarts | `debugger_restarts` | thread |
| Invoke restart | `debugger_invoke_restart` | thread, restart, args |
| Set breakpoint | `breakpoint_set` | function, condition, hit-count |
| Remove breakpoint | `breakpoint_remove` | breakpoint-id |
| Step execution | `debugger_step` | thread, mode |
| Continue | `debugger_continue` | thread |

## Core Debugging Philosophy

Common Lisp's debugger is fundamentally different from traditional debuggers:
- **Condition System**: Errors raise conditions, not crash immediately
- **Restarts**: Users choose how to recover from errors
- **Interactive**: Debugger is a REPL with full Lisp power
- **Non-intrusive**: Debugging doesn't require special compilation

## Workflow 1: Post-Mortem Debugging

When an error occurs, capture and analyze the context.

### Step 1: Capture Error Context

```json
{
  "tool": "debugger_frames",
  "arguments": {
    "thread": "auto"
  }
}
```

This returns:
- Frame count and details
- Function names for each frame
- Source locations (file, line, column)
- For each frame: local variables with values and object IDs

### Step 2: Inspect Error Condition

```json
{
  "tool": "debugger_restarts",
  "arguments": {
    "thread": "auto"
  }
}
```

Returns available restarts:
- `ABORT`: Exit debugger, return to top level
- `USE-VALUE`: Provide a value to use instead
- `STORE-VALUE`: Store a value for future use
- `RETRY`: Retry the operation

### Step 3: Examine Local Variables

For frame 0 (error location):
```json
{
  "tool": "debugger_frame_locals",
  "arguments": {
    "frame": 0
  }
}
```

For non-primitive values, use the returned `object_id`:
```json
{
  "tool": "inspect_object",
  "arguments": {
    "id": 42,
    "max_depth": 3
  }
}
```

### Step 4: Evaluate in Frame Context

```json
{
  "tool": "debugger_eval_in_frame",
  "arguments": {
    "frame": 0,
    "code": "(type-of x)"
  }
}
```

This evaluates in the lexical environment of that frame.

### Step 5: Invoke Restart to Recover

```json
{
  "tool": "debugger_invoke_restart",
  "arguments": {
    "restart": "USE-VALUE",
    "frame": 0,
    "args": ["new-value"]
  }
}
```

## Workflow 2: Live Debugging

Attach debugger to a running process or set breakpoints proactively.

### Step 1: Start Debug Server

```lisp
;; In the running Lisp image
(ql:quickload :cl-tron-mcp)
(cl-tron-mcp:start-server :transport :stdio)
```

### Step 2: List Threads

```json
{
  "tool": "thread_list"
}
```

Returns all threads with their states.

### Step 3: Attach Debugger to Thread

```json
{
  "tool": "thread_debug",
  "arguments": {
    "thread-id": "T1-12345"
  }
}
```

### Step 4: Set Breakpoint

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-package:process-data",
    "condition": "(> x 100)"
  }
}
```

Returns breakpoint ID for later removal.

### Step 5: Step Through Code

```json
{
  "tool": "debugger_step",
  "arguments": {
    "thread": "auto",
    "mode": "into"
  }
}
```

Modes:
- `into`: Step into function calls
- `over`: Step over function calls
- `out`: Step out of current function

### Step 6: Continue Execution

```json
{
  "tool": "debugger_continue",
  "arguments": {
    "thread": "auto"
  }
}
```

## Workflow 3: Interactive Object Inspection

Deep inspection of Lisp objects during debugging.

### Basic Inspection

```json
{
  "tool": "inspect_object",
  "arguments": {
    "id": 42
  }
}
```

Returns object type and all slots/elements.

### Inspect CLOS Instance

```json
{
  "tool": "inspect_slot",
  "arguments": {
    "object-id": 42,
    "slot-name": "cache"
  }
}
```

### Inspect Function

```json
{
  "tool": "inspect_function",
  "arguments": {
    "symbol": "my-package:compute"
  }
}
```

Returns:
- Function name
- Lambda list
- Documentation string
- Closure variables (if applicable)

### Inspect Package

```json
{
  "tool": "inspect_package",
  "arguments": {
    "package": "MY-APP"
  }
}
```

Returns:
- Package nicknames
- Use list
- Export list
- Internal symbols

## Workflow 4: Conditional Debugging

Set up complex debugging scenarios.

### Break with Condition

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:handle-request",
    "condition": "(equal (car args) \"admin\")"
  }
}
```

### Break on Hit Count

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:process-item",
    "hit-count": 10
  }
}
```

### Break on Thread

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:worker",
    "thread": "worker-1"
  }
}
```

## Workflow 5: Recovery and Continuation

After debugging, continue execution safely.

### Use Restart to Continue

```json
{
  "tool": "debugger_invoke_restart",
  "arguments": {
    "restart": "CONTINUE"
  }
}
```

### Return Value from Frame

```json
{
  "tool": "debugger_return_from_frame",
  "arguments": {
    "frame": 5,
    "value": "(list result)"
  }
}
```

### Fix and Retry

```json
{
  "tool": "debugger_eval_in_frame",
  "arguments": {
    "frame": 0,
    "code": "(setf x 10)"
  }
}
```

Then invoke RETRY restart.

## Common Scenarios

### Scenario: Null Pointer Equivalent

Lisp uses `NIL` which can mean "not found" or "no value":

```json
{
  "tool": "debugger_eval_in_frame",
  "arguments": {
    "frame": 0,
    "code": "(type-of problematic-var)"
  }
}
```

Check if it's:
- `NIL` (false/no value)
- Empty list `()`
- Unbound slot (error condition)

### Scenario: Type Error

```json
{
  "tool": "debugger_eval_in_frame",
  "arguments": {
    "frame": 0,
    "code": "(list (type-of x) x)"
  }
}
```

Examine actual types and values.

### Scenario: Infinite Loop

Set breakpoint in suspected loop:

```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:loop-body",
    "hit-count": 1
  }
}
```

Then use `debugger_step` to examine each iteration.

### Scenario: Memory Issues

```json
{
  "tool": "memory_stats"
}
```

Check generation sizes and GC frequency.

## Error Handling

### Debugger Already Active

If debugger is already active in a thread:
- Use `thread_list` to find other threads
- Debug from a different thread
- Use `debugger_continue` to exit first

### No Error Context

If no error occurred but you want to inspect:
```json
{
  "tool": "debugger_frames",
  "arguments": {
    "thread": "current"
  }
}
```

### Permission Denied

Some operations require approval:
- `debugger_eval_in_frame` - can modify state
- `breakpoint_set` - modifies running code
- `thread_kill` - terminates threads

Request approval before proceeding.

## Best Practices

1. **Capture full context first**: Get frames, locals, restarts before making changes
2. **Inspect before modifying**: Understand state before intervening
3. **Use restarts when possible**: Prefer restarts over frame manipulation
4. **Document findings**: Note what caused the issue
5. **Fix the root cause**: Use hot-reload tools after diagnosis
6. **Verify the fix**: Re-run to confirm the issue is resolved

## See Also

- @prompts/hot-reload-development.md - Fixing code while running
- @agents/sbcl-debugging-expert.md - Deep debugging strategies
- docs/tools/debugger.md - Complete debugger tool reference
