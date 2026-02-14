# Swank Integration

This document describes the Swank client implementation that enables the MCP to interact with a running SBCL+Swank session exactly like Slime does.

## Overview

The Swank integration provides a **proper Swank protocol client** that connects to an external Swank server and uses the full range of Swank RPC operations:

- **Evaluation**: `swank-eval`, `swank-compile`
- **Debugging**: `swank-backtrace`, `swank-frame-locals`, `swank-eval-in-frame`
- **Restarts**: `swank-get-restarts`, `swank-invoke-restart`
- **Stepping**: `swank-step`, `swank-next`, `swank-out`, `swank-continue`
- **Breakpoints**: `swank-set-breakpoint`, `swank-remove-breakpoint`, `swank-list-breakpoints`
- **Inspection**: `swank-inspect-object`, `swank-describe`
- **Documentation**: `swank-autodoc`, `swank-completions`
- **Threads**: `swank-threads`, `swank-abort-thread`, `swank-interrupt`

## Architecture

### Protocol Layer (`src/swank/protocol.lisp`)

Implements the Swank wire protocol:

- **Length-prefixed framing**: Each message is preceded by a 6-character hexadecimal length
- **UTF-8 encoding/decoding**: Converts between strings and byte vectors
- **Message I/O**: `read-packet`, `write-message`

```
Example message: "000039(:emacs-rex (swank:connection-info) nil t 1)"
                 ^^^^^^ 6-char hex length
```

### Client Layer (`src/swank/client.lisp`)

Implements the full Swank client:

1. **Connection Management**
   - Socket connection via usocket
   - Dedicated reader thread for async messages
   - Event processor thread for debugger events

2. **Request-Response Correlation**
   - Unique integer IDs for each request
   - Pending requests hash table
   - Condition variables for synchronous RPC calls

3. **Event Queue**
   - Async events (`:debug`, `:write-string`, etc.) are queued
   - Debugger events captured for later inspection

### Symbol Resolution

The Swank package is loaded **at runtime** on the remote Lisp, not in the MCP process. All Swank RPC symbols are resolved dynamically via `swank-sym`:

```lisp
(defun swank-sym (name)
  "Find symbol NAME in the SWANK package at runtime."
  (let ((pkg (find-package :swank)))
    (unless pkg
      (error "SWANK package not loaded."))
    (let ((sym (find-symbol (string-upcase name) pkg)))
      (unless sym
        (error "Symbol ~a not found in SWANK package" name))
      sym)))
```

This allows the MCP to compile without requiring Swank to be loaded locally.

## Message Flow

```
┌─────────────────┐     ┌─────────────────┐
│   MCP Client    │     │  Swank Server   │
│   (MCP proc)    │     │  (Lisp proc)    │
└────────┬────────┘     └────────┬────────┘
         │                       │
         │  :emacs-rex REQ       │
         │──────────────────────>│
         │                       │
         │  :return RESP         │
         │<──────────────────────│
         │                       │
         │  :debug EVENT         │
         │<──────────────────────│
         │                       │
```

### Request Format

```lisp
(:emacs-rex FORM PACKAGE THREAD ID)
```

- `FORM`: The S-expression to evaluate (e.g., `(swank:backtrace 0 20)`)
- `PACKAGE`: Package name string for evaluation context
- `THREAD`: Thread specifier (`t`, `:repl-thread`, or integer)
- `ID`: Unique request ID for correlation

### Response Format

```lisp
(:return THREAD RESULT ID)
```

### Async Event Format

```lisp
(:debug THREAD LEVEL CONDITION RESTARTS FRAMES)
```

## Usage

### Starting a Swank Server

In your Lisp process:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4005)
```

### Connecting from MCP

Via MCP tool call:

```json
{
  "name": "swank_connect",
  "arguments": {"host": "127.0.0.1", "port": 4005}
}
```

Or using the unified interface:

```json
{
  "name": "repl_connect",
  "arguments": {"type": "swank", "port": 4005}
}
```

### Example Debugging Session

```json
// 1. Evaluate code that triggers debugger
{"name": "repl_eval", "arguments": {"code": "(/ 1 0)"}}

// 2. Get backtrace
{"name": "repl_backtrace"}

// 3. Get available restarts
{"name": "repl_get_restarts"}

// 4. Invoke a restart
{"name": "repl_invoke_restart", "arguments": {"restartIndex": 1}}

// 5. Continue execution
{"name": "repl_continue"}
```

## RPC Operations Reference

### Evaluation

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-eval` | `swank:eval` | Evaluate code in package |
| `swank-compile` | `swank:compile-string-for-emacs` | Compile code string |

### Debugging

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-backtrace` | `swank:backtrace` | Get stack frames |
| `swank-frame-locals` | `swank:frame-locals-and-catch-tags` | Get frame locals |
| `swank-eval-in-frame` | `swank:eval-string-in-frame` | Eval in frame context |

### Restarts

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-get-restarts` | `swank:compute-restarts-for-emacs` | List restarts |
| `swank-invoke-restart` | `swank:invoke-nth-restart` | Invoke restart by index |

### Stepping

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-step` | `swank:sldb-step-into` | Step into |
| `swank-next` | `swank:sldb-step-next` | Step over |
| `swank-out` | `swank:sldb-step-out` | Step out |
| `swank-continue` | `swank:sldb-continue` | Continue execution |

### Breakpoints

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-set-breakpoint` | `swank:break` | Set breakpoint |
| `swank-remove-breakpoint` | `swank:break-remove` | Remove breakpoint |
| `swank-list-breakpoints` | `swank:break-list` | List breakpoints |

### Threads

| Function | Swank RPC | Description |
|----------|-----------|-------------|
| `swank-threads` | `swank:thread-list` | List threads |
| `swank-abort-thread` | `swank:abort-thread` | Abort thread |
| `swank-interrupt` | `swank:interrupt` | Interrupt evaluation |

## Thread Safety

The client uses multiple threads:

1. **Main Thread**: Handles MCP requests, sends Swank RPCs
2. **Reader Thread**: Continuously reads incoming messages, dispatches to handlers
3. **Event Processor**: Processes async events (currently logs them)

All shared state uses mutexes and condition variables from bordeaux-threads.

## Error Handling

- Connection errors return `(:error t :message "...")`
- RPC errors are captured in the response
- Timeout on requests returns `(:error t :message "Request timeout")`

## See Also

- [architecture.md](architecture.md) - Overall architecture
- [tools/debugger.md](tools/debugger.md) - Debugger tool documentation
- [AGENTS.md](../AGENTS.md) - Agent guidelines
