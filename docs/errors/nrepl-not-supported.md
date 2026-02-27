# NREPL_NOT_SUPPORTED

**Error Code:** `NREPL_NOT_SUPPORTED`

**Message:** NREPL is not supported

## Description

This error occurs when attempting to use NREPL (Clojure's network REPL) functionality. The Tron MCP server currently only supports Swank (SBCL's protocol) and does not support NREPL.

## Common Causes

- Attempting to connect to an NREPL server
- Specifying `:type :nrepl` in connection parameters
- Using NREPL-specific tools or commands

## Resolution

1. Use Swank instead of NREPL
2. Start SBCL with Swank server enabled:
   ```lisp
   (ql:quickload :swank)
   (swank:create-server :port 4006)
   ```
3. Connect using `repl_connect` with default or `:type :swank`

## Example

```lisp
;; Start Swank server in SBCL
(ql:quickload :swank)
(swank:create-server :port 4006)

;; Connect from Tron
(repl_connect :host "localhost" :port 4006)
```

## Related Tools

- `repl_connect` - Connect to Swank REPL
- `swank_connect` - Connect to Swank server