# swank_next

**Short Description:** Step over next expression

**Full Description:** Step over the next expression in the debugger. REQUIRES: swank_connect first. Evaluates the next form without stepping into function calls.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(swank_next :frame 0)
```

**Notes:** Only works when in an active stepping context. Steps over function calls, executing them without tracing into their internals.