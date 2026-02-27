# REPL_NOT_CONNECTED

## Error Code
`REPL_NOT_CONNECTED`

## Message
Not connected to any REPL

## Hint
Run repl_connect first. Example: repl_connect :port 4006

## Setup Instructions
To start Swank in SBCL:
```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

## Common Causes
- The MCP server was started without connecting to a REPL
- The REPL connection was disconnected
- The Swank server is not running

## Resolution Steps
1. Start Swank in your SBCL session (see setup instructions above)
2. Connect using `repl_connect :port 4006`
3. Verify connection with `repl_status`

## Related Tools
- `repl_connect` - Connect to a REPL
- `repl_status` - Check connection status
- `repl_disconnect` - Disconnect from REPL

## See Also
- [Getting Started Guide](../agents/getting-started.md)
- [Swank Integration](../swank-integration.md)