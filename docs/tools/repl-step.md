# repl_step

**Short Description:** Step into next expression

**Full Description:** Step into the next expression. REQUIRES: repl_connect and an active stepping context (entered via (step ...)). Shows the next form to be evaluated.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(repl_step :frame 0)
```

**Notes:** Only works when in an active stepping context. Steps into function calls, allowing you to trace execution into called functions.