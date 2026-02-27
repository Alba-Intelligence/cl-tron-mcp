# Protocol Handlers

This document describes the JSON-RPC 2.0 protocol handlers for the MCP server.

## Overview

The protocol handlers are responsible for processing incoming JSON-RPC requests and routing them to the appropriate handler functions. The handlers are organized into modular files for better maintainability.

## File Structure

The protocol handlers are split into the following files:

### Core Files

- **`handlers.lisp`** (95 lines) - Main entry point with message dispatch and routing
- **`handlers-utils.lisp`** (197 lines) - Validation, error recovery, and utility functions

### Handler Modules

- **`handlers-initialize.lisp`** (38 lines) - Initialize handler for server handshake
- **`handlers-tools.lisp`** (197 lines) - Tools handlers with approval workflow
- **`handlers-resources.lisp`** (42 lines) - Resources handlers (list, read)
- **`handlers-prompts.lisp`** (42 lines) - Prompts handlers (list, get)
- **`handlers-ping.lisp`** (23 lines) - Ping handler for keepalive

## Message Dispatch

The main `handle-message` function in `handlers.lisp` dispatches requests based on the JSON-RPC method:

```lisp
(defun handle-message (message)
  "Handle incoming JSON-RPC message.
Dispatches to appropriate handler based on method name."
  (handler-case (let* ((parsed (jonathan:parse message))
                       (id (getf parsed :|id|))
                       (method (getf parsed :|method|))
                       (params (getf parsed :|params|)))
                  (cond
                   ((null id)
                    (handle-notification method params))
                   (t (handle-request id method params))))))
```

## Supported Methods

### Core Protocol

- `initialize` - Server handshake and capability negotiation
- `ping` - Keepalive

### Tools

- `tools/list` - List available tools
- `tools/call` - Invoke a tool (with approval workflow)
- `approval/respond` - Respond to approval requests

### Resources

- `resources/list` - List available documentation files
- `resources/read` - Read a documentation file by URI

### Prompts

- `prompts/list` - List available guided workflows
- `prompts/get` - Get a specific prompt with instructions

## Tool Approval Workflow

The `tools/call` handler implements a server-enforced approval workflow:

1. **Check for approval re-invocation** - If the request includes `approval_request_id` and `approved: true`, consume the approval and run the tool
2. **Check if approval required** - If the tool requires approval and is not whitelisted, return `approval_required` response
3. **Execute tool** - If no approval needed or whitelisted, execute the tool with timeout

### Helper Functions

- `arguments-without-approval-params` - Remove approval parameters from arguments
- `check-tool-approval` - Check if tool requires approval and return approval_required if needed
- `handle-approval-reinvocation` - Handle re-invocation with approval from client
- `execute-tool-with-timeout` - Execute tool with timeout and error handling

## Parameter Validation

All handlers use validation functions from `handlers-utils.lisp`:

- `validate-string-param` - Validate string parameters
- `validate-list-param` - Validate list parameters
- `validate-integer-param` - Validate integer parameters
- `validate-boolean-param` - Validate boolean parameters

Each validation function returns a plist with `:valid` key and optional `:error` key.

## Error Handling

### Error Recovery

The `cleanup-on-error` function performs cleanup when errors occur:

- Logs the error
- Disconnects from Swank if connected
- Clears pending requests
- Logs cleanup completion

### Error Responses

The `make-error-response` function creates JSON-RPC error responses with:

- Standard JSON-RPC error codes (-32600 to -32603)
- Server-defined error codes (-32000 to -32099)
- Optional error details (code, hint, param, min-length, min)

### Timeout Handling

The `with-timeout` macro executes code with a timeout:

```lisp
(with-timeout (30)
  ;; Code that should complete within 30 seconds
  ...)
```

If timeout occurs, a `timeout-error` condition is signaled.

## Global State

The following global variables are used:

- `*message-handler*` - Current message handler
- `*request-id*` - Current request ID
- `*default-tool-timeout*` - Default timeout for tool execution (30 seconds)
- `*pending-requests*` - Hash table tracking pending requests for cleanup
- `*request-lock*` - Lock for synchronizing access to `*pending-requests*`

## See Also

- [MCP Resources and Prompts](mcp-resources-prompts.md) - MCP protocol documentation
- [Architecture](architecture.md) - Overall system architecture
- [Tool Documentation](tools/) - Individual tool documentation
