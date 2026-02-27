# repl_status

**Short Description:** Check REPL connection status

**Full Description:** Check REPL connection status and type. Shows :connected, :type (:swank), :host, and :port. Use to verify connection before using other REPL tools.

**Parameters:** None

**Returns:** Status object with connection details

**Example Usage:**
```lisp
(repl_status)
```

**Notes:** Useful for debugging connection issues and verifying the REPL is ready for use.