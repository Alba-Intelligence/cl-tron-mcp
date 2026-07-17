# swank_invoke_restart

**Short Description:** Invoke a restart by index (0-based; 0 = the first/CONTINUE restart)

**Full Description:** Invoke a restart by index to recover from an error. REQUIRES: swank_connect first. Use swank_get_restarts to see available options. Index is 0-based: 0 is the first restart shown in the debugger's restart list (typically CONTINUE), 1 the second, and so on — matching the order in the :debug payload and Swank's INVOKE-NTH-RESTART-FOR-EMACS.

**Parameters:**

- `restart_index`: Restart index to invoke (required, 0-based; 0 = first/CONTINUE restart)

**Returns:** Result of invoking the restart

**Example Usage:**

```lisp
(swank_invoke_restart :restart_index 0)
```

**Notes:** Only works when in an active debugger session. Use swank_get_restarts first to see available restarts and their indices.
