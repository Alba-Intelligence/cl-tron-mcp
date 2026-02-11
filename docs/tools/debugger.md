# Debugger Tools

Tools for debugging Common Lisp applications at runtime.

## Tools Overview

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `debugger_frames` | Get stack frames | No |
| `debugger_frame_locals` | Get local variables in frame | No |
| `debugger_eval_in_frame` | Evaluate in frame context | Yes (:eval) |
| `debugger_restarts` | List available restarts | No |
| `debugger_invoke_restart` | Invoke a restart | Yes (:modify-restarts) |
| `breakpoint_set` | Set breakpoint | Yes (:set-breakpoint) |
| `breakpoint_remove` | Remove breakpoint | No |
| `breakpoint_list` | List all breakpoints | No |
| `debugger_step` | Step through code | Yes (:modify-running-code) |
| `debugger_continue` | Continue execution | No |

## debugger_frames

### Overview
Get the current debugger stack frames for a thread.

### Tool Definition
```json
{
  "name": "debugger_frames",
  "description": "Get current debugger stack frames",
  "parameters": {
    "type": "object",
    "properties": {
      "thread": {
        "type": "string",
        "description": "Thread ID or 'auto' for current"
      },
      "start": {
        "type": "integer",
        "description": "Start frame index",
        "default": 0
      },
      "end": {
        "type": "integer",
        "description": "End frame index",
        "default": 20
      }
    },
    "required": []
  }
}
```

### Return Value
```json
{
  "frames": [
    {
      "index": 0,
      "function": "MY-APP:COMPUTE",
      "source": {
        "file": "src/compute.lisp",
        "line": 42,
        "column": 10
      },
      "locals": [
        {
          "name": "X",
          "value": "10",
          "object_id": 42
        }
      ]
    }
  ],
  "total_frames": 5
}
```

## debugger_eval_in_frame

### Overview
Evaluate Lisp code in the lexical environment of a specific frame.

### Tool Definition
```json
{
  "name": "debugger_eval_in_frame",
  "description": "Evaluate expression in frame's lexical environment",
  "parameters": {
    "type": "object",
    "properties": {
      "frame": {
        "type": "integer",
        "description": "Frame index"
      },
      "code": {
        "type": "string",
        "description": "Lisp code to evaluate"
      },
      "thread": {
        "type": "string",
        "description": "Thread ID or 'auto'"
      }
    },
    "required": ["frame", "code"]
  }
}
```

### Approval Required
This tool requires approval for `:eval` operation.

### User Prompt
```
AI agent wants to evaluate code in frame #2:
Code: (type-of x)

Allow? [Yes/No/Show Code] Timeout: 60s
```

## breakpoint_set

### Overview
Set a breakpoint on a function or source location.

### Tool Definition
```json
{
  "name": "breakpoint_set",
  "description": "Set breakpoint on function",
  "parameters": {
    "type": "object",
    "properties": {
      "function": {
        "type": "string",
        "description": "Function symbol (package:name)"
      },
      "condition": {
        "type": "string",
        "description": "Lisp condition for conditional breakpoint"
      },
      "hit_count": {
        "type": "integer",
        "description": "Break after N hits"
      },
      "thread": {
        "type": "string",
        "description": "Thread ID for thread-specific breakpoint"
      }
    },
    "required": ["function"]
  }
}
```

### Approval Required
This tool requires approval for `:set-breakpoint` operation.

### Usage Examples

**Simple breakpoint:**
```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:process-item"
  }
}
```

**Conditional breakpoint:**
```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:handle-request",
    "condition": "(equal (car args) \"admin\")"
  }
}
```

**Hit count breakpoint:**
```json
{
  "tool": "breakpoint_set",
  "arguments": {
    "function": "my-app:loop-body",
    "hit_count": 10
  }
}
```

## Error Handling

### Common Errors

**Thread not in debugger:**
```json
{
  "error": {
    "code": -32000,
    "message": "Thread not in debugger",
    "data": {
      "type": "NOT_DEBUGGING"
    }
  }
}
```

**Invalid frame index:**
```json
{
  "error": {
    "code": -32000,
    "message": "Frame index out of range",
    "data": {
      "type": "INVALID_FRAME"
    }
  }
}
```

## See Also

- @prompts/debugging-workflows.md - Complete debugging workflows
- @agents/sbcl-debugging-expert.md - Debugging strategies
