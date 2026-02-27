# INTERNAL_ERROR

**Error Code:** `INTERNAL_ERROR`

**Message:** Internal error: {error_message}

## Description

This error occurs when an unexpected error happens within the MCP server. This indicates a bug or unexpected condition that should be reported.

## Common Causes

- Bug in the MCP server code
- Unexpected state or condition
- Resource exhaustion
- Unhandled exception

## Resolution

1. Check server logs for detailed error information
2. Report the issue with full error details
3. Try restarting the server
4. Check system resources

## Example

```lisp
;; Check server logs for details
;; In server logs, look for:
;; [ERROR] Internal error: <detailed error message>

;; Restart server if needed
(cl-tron-mcp/core:stop-server)
(cl-tron-mcp/core:start-server)
```

## Related Tools

- All MCP tools
- Server management
- Logging tools