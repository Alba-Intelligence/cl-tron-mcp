# swank_disconnect

**Short Description:** Disconnect from Swank

**Full Description:** Disconnect from the Swank server. The SBCL session continues running - only the MCP connection is closed.

**Parameters:** None

**Returns:** Disconnection status

**Example Usage:**
```lisp
(swank_disconnect)
```

**Notes:** Does not affect the running SBCL session. Can reconnect later with swank_connect.