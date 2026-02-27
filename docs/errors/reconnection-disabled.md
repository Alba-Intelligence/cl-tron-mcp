# RECONNECTION_DISABLED

**Error Code:** `RECONNECTION_DISABLED`

**Message:** Reconnection is disabled

## Description

This error occurs when attempting to use reconnection functionality when it has been explicitly disabled in the configuration.

## Common Causes

- Reconnection was disabled in configuration
- System is configured to not auto-reconnect
- Manual reconnection is required

## Resolution

1. Enable reconnection in configuration if needed
2. Manually reconnect using `swank_connect` or `repl_connect`
3. Check configuration settings

## Example

```lisp
;; Manually reconnect
(swank_connect :host "localhost" :port 4006)

;; Or enable reconnection in config and restart
```

## Related Tools

- `swank_connect` - Connect to Swank server
- `repl_connect` - Connect to REPL
- Configuration management