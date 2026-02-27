# REPL_ALREADY_CONNECTED

**Error Code:** `REPL_ALREADY_CONNECTED`

**Message:** REPL is already connected

## Description

This error occurs when attempting to connect to a REPL when a connection is already active. The system only supports one active REPL connection at a time.

## Common Causes

- Calling `repl_connect` when already connected to a REPL
- Attempting to reconnect without first disconnecting
- Multiple connection attempts in quick succession

## Resolution

1. Check if REPL is connected: `repl_status`
2. Disconnect first: `repl_disconnect`
3. Then reconnect: `repl_connect`

## Example

```lisp
;; Check current status
(repl_status)
;; => {:connected true, :type :swank, :host "localhost", :port 4006}

;; Disconnect first
(repl_disconnect)

;; Now reconnect
(repl_connect :host "localhost" :port 4006)
```

## Related Tools

- `repl_connect` - Connect to REPL
- `repl_disconnect` - Disconnect from REPL
- `repl_status` - Check connection status