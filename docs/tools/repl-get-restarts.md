# repl_get_restarts

**Short Description:** Get available restarts

**Full Description:** Get available restarts for error recovery. REQUIRES: repl_connect and an active debugger session. Shows options like ABORT, RETRY, USE-VALUE.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** List of available restarts with indices and descriptions

**Example Usage:**
```lisp
(repl_get_restarts :frame 0)
```

**Notes:** Only works when in an active debugger session. Restart indices are 1-based. Use repl_invoke_restart to invoke a specific restart.