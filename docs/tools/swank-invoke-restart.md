# swank_invoke_restart

**Short Description:** Invoke a restart

**Full Description:** Invoke a restart by index to recover from an error. REQUIRES: swank_connect first. Use swank_get_restarts to see available options. Index is 1-based.

**Parameters:**
- `restart_index`: Restart index to invoke (required, 1-based)

**Returns:** Result of invoking the restart

**Example Usage:**
```lisp
(swank_invoke_restart :restart_index 1)
```

**Notes:** Only works when in an active debugger session. Use swank_get_restarts first to see available restarts and their indices.