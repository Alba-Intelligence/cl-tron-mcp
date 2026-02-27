# SWANK_ALREADY_CONNECTED

**Error Code:** `SWANK_ALREADY_CONNECTED`

**Message:** Swank is already connected

## Description

This error occurs when attempting to connect to a Swank server when a connection is already active. The system only supports one active Swank connection at a time.

## Common Causes

- Calling `swank_connect` when already connected to Swank
- Attempting to reconnect without first disconnecting
- Multiple connection attempts in quick succession

## Resolution

1. Check if Swank is connected: `swank_status`
2. Disconnect first: `swank_disconnect`
3. Then reconnect: `swank_connect`

## Example

```lisp
;; Check current status
(swank_status)
;; => {:connected true, :host "localhost", :port 4006}

;; Disconnect first
(swank_disconnect)

;; Now reconnect
(swank_connect :host "localhost" :port 4006)
```

## Related Tools

- `swank_connect` - Connect to Swank server
- `swank_disconnect` - Disconnect from Swank
- `swank_status` - Check connection status