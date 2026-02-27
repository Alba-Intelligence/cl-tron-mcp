# repl_next

**Short Description:** Step over next expression

**Full Description:** Step over the next expression. REQUIRES: repl_connect and an active stepping context. Evaluates the next form without stepping into function calls.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(repl_next :frame 0)
```

**Notes:** Only works when in an active stepping context. Steps over function calls, executing them without tracing into their internals.