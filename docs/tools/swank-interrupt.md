# swank_interrupt

**Short Description:** Interrupt current thread

**Full Description:** Interrupt the current thread's execution in the connected SBCL. REQUIRES: swank_connect first. Triggers the debugger for long-running computations.

**Parameters:** None

**Returns:** Interrupt status

**Example Usage:**
```lisp
(swank_interrupt)
```

**Notes:** Requires user approval. Interrupts the current thread and enters the debugger. Useful for stopping long-running computations.