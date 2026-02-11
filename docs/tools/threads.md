# Thread Debugging Tools

Tools for managing and debugging multi-threaded SBCL applications.

## Tools Overview

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `thread_list` | List all threads | No |
| `thread_inspect` | Inspect thread state | No |
| `thread_debug` | Attach debugger | Yes (:modify-restarts) |
| `thread_interrupt` | Send interrupt | Yes (:terminate-thread) |
| `thread_kill` | Terminate thread | Yes (:terminate-thread) |
| `thread_create` | Create new thread | Yes (:modify-running-code) |
| `thread_dump_all` | Get all thread stacks | No |

## thread_list

### Overview
List all threads with their status.

### Tool Definition
```json
{
  "name": "thread_list",
  "description": "List all threads with status"
}
```

### Return Value
```json
{
  "threads": [
    {
      "id": "T1-12345",
      "name": "worker-1",
      "state": ":running",
      "priority": 0
    },
    {
      "id": "T1-12346",
      "name": "repl",
      "state": ":waiting",
      "priority": 0
    }
  ]
}
```

## thread_kill

### Overview
Terminate a thread. Use with caution.

### Tool Definition
```json
{
  "name": "thread_kill",
  "description": "Terminate thread",
  "parameters": {
    "type": "object",
    "properties": {
      "thread_id": {
        "type": "string",
        "description": "Thread ID to terminate"
      }
    },
    "required": ["thread_id"]
  }
}
```

### Approval Required
Requires `:terminate-thread` approval.

### User Prompt
```
AI agent wants to terminate thread "worker-2":
Reason: Thread stuck in infinite loop

Allow? [Yes/No] Timeout: 30s
```

## See Also

- @prompts/debugging-workflows.md - Thread debugging scenarios
