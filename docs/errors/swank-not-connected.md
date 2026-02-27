# SWANK_NOT_CONNECTED

**Error Code:** `SWANK_NOT_CONNECTED`

**Message:** Swank is not connected

## Description

This error occurs when attempting to use Swank functionality without an active connection to a Swank server.

## Common Causes

- Calling Swank tools before connecting
- Connection was lost or disconnected
- Server was stopped or crashed

## Resolution

1. Connect to Swank server: `swank_connect`
2. Verify Swank server is running in SBCL:
   ```lisp
   (ql:quickload :swank)
   (swank:create-server :port 4006)
   ```
3. Check connection status: `swank_status`

## Example

```lisp
;; Start Swank server in SBCL
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)

;; Connect from Tron
(swank_connect :host "localhost" :port 4006)

;; Now use Swank tools
(swank_eval :code "(+ 1 2)")
```

## Related Tools

- `swank_connect` - Connect to Swank server
- `swank_status` - Check connection status
- All Swank tools (swank_eval, swank_backtrace, etc.)