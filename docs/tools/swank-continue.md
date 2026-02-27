# swank_continue

**Short Description:** Continue from debugger

**Full Description:** Continue execution from the debugger. REQUIRES: swank_connect first. Resumes normal execution after an error (if the condition is continuable).

**Parameters:** None

**Returns:** Execution status

**Example Usage:**
```lisp
(swank_continue)
```

**Notes:** Only works when in an active debugger session. Resumes normal program execution from the current point.