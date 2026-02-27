# swank_step

**Short Description:** Step into next expression

**Full Description:** Step into the next expression in the debugger. REQUIRES: swank_connect first. Use when in a stepping context (entered via (step ...)). Shows the next form to be evaluated.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(swank_step :frame 0)
```

**Notes:** Only works when in an active stepping context. Steps into function calls, allowing you to trace execution into called functions.