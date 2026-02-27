# repl_continue

**Short Description:** Continue from debugger

**Full Description:** Continue execution from the debugger. REQUIRES: repl_connect and an active debugger session. Resumes normal execution.

**Parameters:** None

**Returns:** Execution status

**Example Usage:**
```lisp
(repl_continue)
```

**Notes:** Only works when in an active debugger session. Resumes normal program execution from the current point.