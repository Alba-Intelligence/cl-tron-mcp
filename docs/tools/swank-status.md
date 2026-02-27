# swank_status

**Short Description:** Get Swank connection status

**Full Description:** Get the current Swank connection status. Shows if connected, reader thread status, and event processor status. Use to verify connection before using other Swank tools.

**Parameters:** None

**Returns:** Status object with connection details

**Example Usage:**
```lisp
(swank_status)
```

**Notes:** Useful for debugging connection issues and verifying the Swank connection is ready for use.