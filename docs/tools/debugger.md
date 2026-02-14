# Debugger Tools

Tools for debugging Common Lisp applications at runtime via Swank connection.

## Prerequisites

The debugger tools require an active Swank connection. Connect first:

```json
{"name": "swank_connect", "arguments": {"port": 4005}}
```

Or use the unified interface:

```json
{"name": "repl_connect", "arguments": {"type": "swank", "port": 4005}}
```

## Tools Overview

### Core Debugger Tools

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `debugger_frames` | Get stack frames | No |
| `debugger_restarts` | List available restarts | No |
| `step_frame` | Step execution (into/over/out) | No |
| `breakpoint_set` | Set breakpoint on function | Yes |
| `breakpoint_remove` | Remove breakpoint by ID | No |
| `breakpoint_list` | List all breakpoints | No |

### Unified REPL Debugger Tools

| Tool | Purpose | Swank Backend |
|------|---------|---------------|
| `repl_backtrace` | Get current backtrace | `swank:backtrace` |
| `repl_frame_locals` | Get frame variables | `swank:frame-locals-and-catch-tags` |
| `repl_get_restarts` | List available restarts | `swank:compute-restarts-for-emacs` |
| `repl_invoke_restart` | Invoke restart by index | `swank:invoke-nth-restart` |
| `repl_step` | Step into next expression | `swank:sldb-step-into` |
| `repl_next` | Step over next expression | `swank:sldb-step-next` |
| `repl_out` | Step out of current frame | `swank:sldb-step-out` |
| `repl_continue` | Continue execution | `swank:sldb-continue` |
| `repl_set_breakpoint` | Set breakpoint | `swank:break` |
| `repl_remove_breakpoint` | Remove breakpoint | `swank:break-remove` |
| `repl_list_breakpoints` | List breakpoints | `swank:break-list` |

## debugger_frames

Get the current debugger stack frames.

### Parameters

| Name | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| thread | string | No | nil | Thread ID |
| start | integer | No | 0 | Start frame index |
| end | integer | No | 20 | End frame index |

### Example Request

```json
{
  "name": "debugger_frames",
  "arguments": {"start": 0, "end": 10}
}
```

### Example Response

```json
{
  "frames": [
    {
      "index": 0,
      "function": "MY-APP:COMPUTE-DATA",
      "source": {"file": "src/compute.lisp", "line": 42}
    },
    {
      "index": 1,
      "function": "MY-APP:PROCESS-REQUEST",
      "source": {"file": "src/handler.lisp", "line": 15}
    }
  ],
  "total": 5
}
```

## debugger_restarts

List available debugger restarts.

### Parameters

| Name | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| thread | string | No | nil | Thread ID |

### Example Response

```json
{
  "restarts": [
    {"index": 0, "name": "ABORT", "description": "Return to SLIME's top level"},
    {"index": 1, "name": "RETRY", "description": "Retry the current operation"},
    {"index": 2, "name": "CONTINUE", "description": "Continue from error"}
  ],
  "count": 3
}
```

## step_frame

Step execution in a frame with the specified mode.

### Parameters

| Name | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| frame | integer | Yes | - | Frame index |
| mode | string | No | "into" | Step mode: "into", "over", or "out" |

### Example

```json
// Step into
{"name": "step_frame", "arguments": {"frame": 0, "mode": "into"}}

// Step over
{"name": "step_frame", "arguments": {"frame": 0, "mode": "over"}}

// Step out
{"name": "step_frame", "arguments": {"frame": 0, "mode": "out"}}
```

### Response

```json
{
  "mode": "into",
  "status": "stepping",
  "message": "Stepping into next function call"
}
```

## breakpoint_set

Set a breakpoint on a function.

### Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| functionName | string | Yes | Function name (package:symbol) |
| condition | string | No | Lisp condition for conditional breakpoint |
| hitCount | integer | No | Break after N hits |

### Approval Required

This tool requires user approval for `:set-breakpoint` operation.

### Example

```json
{
  "name": "breakpoint_set",
  "arguments": {
    "functionName": "my-app:process-item",
    "condition": "(> count 10)"
  }
}
```

### Response

```json
{
  "breakpoint-id": 1,
  "function": "my-app:process-item",
  "status": "active",
  "message": "Breakpoint set via Swank"
}
```

## breakpoint_remove

Remove a breakpoint by ID.

### Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| breakpointId | integer | Yes | Breakpoint ID from `breakpoint_set` |

### Example

```json
{
  "name": "breakpoint_remove",
  "arguments": {"breakpointId": 1}
}
```

## breakpoint_list

List all active breakpoints.

### Example Response

```json
{
  "breakpoints": [
    {"id": 1, "function": "my-app:process-item", "enabled": true}
  ],
  "count": 1
}
```

## Debugging Workflow Example

### 1. Trigger the Debugger

```json
{"name": "repl_eval", "arguments": {"code": "(/ 1 0)"}}
```

This triggers a division-by-zero error and enters the debugger.

### 2. Get the Backtrace

```json
{"name": "repl_backtrace"}
```

### 3. Inspect Frame Locals

```json
{"name": "repl_frame_locals", "arguments": {"frame": 0}}
```

### 4. Get Available Restarts

```json
{"name": "repl_get_restarts"}
```

### 5. Invoke a Restart

```json
{"name": "repl_invoke_restart", "arguments": {"restartIndex": 0}}
```

### 6. Continue Execution

```json
{"name": "repl_continue"}
```

## Error Handling

### Not Connected

```json
{
  "error": true,
  "message": "Not connected to any REPL"
}
```

### Swank Package Not Loaded

```json
{
  "error": true,
  "message": "SWANK package not loaded. Connect to a Swank server first."
}
```

### Request Timeout

```json
{
  "error": true,
  "message": "Request timeout"
}
```

## Implementation Notes

The debugger tools use the Swank RPC protocol to communicate with the remote Lisp:

1. **All operations are remote**: The MCP does not access SBCL internals directly
2. **Thread-aware**: Operations can target specific threads via Swank
3. **Async events**: Debugger events are queued and can be processed asynchronously
4. **Symbol resolution**: Swank symbols are resolved at runtime to allow compilation without Swank loaded locally

## See Also

- [swank-integration.md](swank-integration.md) - Swank protocol implementation
- [architecture.md](architecture.md) - Overall architecture
- [../prompts/debugging-workflows.md](../prompts/debugging-workflows.md) - Complete debugging workflows
