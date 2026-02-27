# REPL_DETECTION_FAILED

**Error Code:** `REPL_DETECTION_FAILED`

**Message:** Failed to detect REPL type

## Description

This error occurs when the system cannot automatically determine the type of REPL server running. This typically happens when the server is not responding as expected or is using an unsupported protocol.

## Common Causes

- Server is not running or not accessible
- Server is using an unsupported protocol
- Network connectivity issues
- Firewall blocking the connection
- Server is not a valid REPL server

## Resolution

1. Verify the server is running:
   ```bash
   netstat -an | grep 4006
   ```
2. Check network connectivity:
   ```bash
   telnet localhost 4006
   ```
3. Try connecting with explicit type:
   ```lisp
   (repl_connect :host "localhost" :port 4006 :type :swank)
   ```
4. Check server logs for errors

## Example

```lisp
;; Try explicit connection
(repl_connect :host "localhost" :port 4006 :type :swank)

;; If that fails, verify Swank is running in SBCL
;; In SBCL:
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

## Related Tools

- `repl_connect` - Connect to REPL
- `repl_status` - Check connection status
- `swank_connect` - Connect to Swank server