# repl_disconnect

**Short Description:** Disconnect from REPL

**Full Description:** Disconnect from the current REPL. The Lisp session continues running - only the MCP connection is closed.

**Parameters:** None

**Returns:** Disconnection status object

**Example Usage:**
```lisp
(repl_disconnect)
```

**Notes:** Does not affect the running SBCL session. Can reconnect later with repl_connect.