# RECONNECTION_ERROR

**Error Code:** `RECONNECTION_ERROR`

**Message:** Reconnection error: {error_message}

## Description

This error occurs when an error happens during the reconnection process. The specific error message provides more details about what went wrong.

## Common Causes

- Network connectivity issues
- Server not responding
- Authentication failures
- Port conflicts
- Firewall blocking connection

## Resolution

1. Check the specific error message for details
2. Verify network connectivity
3. Check server status
4. Verify configuration (host, port)
5. Check firewall settings

## Example

```lisp
;; Check connection details
(swank_status)

;; Verify server is running
;; In SBCL:
(swank:server-port)

;; Manually reconnect with correct parameters
(swank_connect :host "localhost" :port 4006)
```

## Related Tools

- `swank_connect` - Connect to Swank server
- `swank_status` - Check connection status
- `repl_connect` - Connect to REPL