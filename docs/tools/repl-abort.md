# repl_abort

**Short Description:** Abort/interrupt REPL evaluation

**Full Description:** Abort/interrupt evaluation in the connected REPL. REQUIRES: repl_connect first. Use when a computation is stuck or taking too long.

**Parameters:** None

**Returns:** Abort status

**Example Usage:**
```lisp
(repl_abort)
```

**Notes:** Interrupts the current evaluation and returns control to the debugger. Use when code is stuck in an infinite loop or long computation.