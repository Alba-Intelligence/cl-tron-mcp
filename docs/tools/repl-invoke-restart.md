# repl_invoke_restart

**Short Description:** Invoke a restart

**Full Description:** Invoke a restart by index to recover from an error. REQUIRES: repl_connect and an active debugger session. Use repl_get_restarts to see available options. Index is 1-based.

**Parameters:**
- `restartIndex`: Restart index to invoke (required, 1-based)

**Returns:** Result of invoking the restart

**Example Usage:**
```lisp
(repl_invoke_restart :restartIndex 1)
```

**Notes:** Only works when in an active debugger session. Use repl_get_restarts first to see available restarts and their indices.