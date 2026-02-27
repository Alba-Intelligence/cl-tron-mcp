# debugger_restarts

**Short Description:** List debugger restarts

**Full Description:** List available debugger restarts. Use to see recovery options after an error. Common restarts: ABORT, RETRY, USE-VALUE, CONTINUE.

**Parameters:** None

**Returns:** List of available restarts with indices and descriptions

**Example Usage:**
```lisp
(debugger_restarts)
```

**Notes:** Only works when in an active debugger session. Restart indices are 1-based.