# SWANK_CONNECTION_FAILED

## Error Code

`SWANK_CONNECTION_FAILED`

## Message

Failed to connect to Swank

## Hint

Ensure Swank server is running and accessible

## Setup Instructions

To start Swank in SBCL:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
```

## Common Causes

- Swank server is not running
- Wrong port number specified
- Network connectivity issues
- Firewall blocking the connection

## Resolution Steps

1. Verify Swank is running in your SBCL session
2. Check the port number matches (default: 4005)
3. Ensure no firewall is blocking the connection
4. Try connecting with `swank_connect :port 4005`

## Example

```lisp
;; Start Swank in SBCL
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)

;; Connect from MCP
(swank_connect :port 4005)
```

## Related Tools

- `swank_connect` - Connect to Swank
- `swank_disconnect` - Disconnect from Swank
- `swank_status` - Check connection status

## See Also

- [Swank Integration Guide](../swank-integration.md)
- [Getting Started](../using/quickstart.md)
