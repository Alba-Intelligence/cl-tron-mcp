# swank_get_restarts

**Short Description:** Get available restarts

**Full Description:** Get available restarts when in the debugger. REQUIRES: swank_connect first. Use after swank_eval triggers an error. Shows recovery options like ABORT, RETRY, USE-VALUE, CONTINUE.

**Parameters:** None

**Returns:** List of available restarts with indices and descriptions

**Example Usage:**
```lisp
(swank_get_restarts)
```

**Notes:** Only works when in an active debugger session. Restart indices are 1-based. Use swank_invoke_restart to invoke a specific restart.