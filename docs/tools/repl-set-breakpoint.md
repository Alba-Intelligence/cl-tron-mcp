# repl_set_breakpoint

**Short Description:** Set a breakpoint

**Full Description:** Set a breakpoint on a function via the connected REPL

**Parameters:**
- `function`: Function name to set breakpoint on (required)
- `condition`: Conditional expression (optional)
- `hitCount`: Number of hits before triggering (optional)
- `thread`: Thread to limit breakpoint to (optional)

**Returns:** Breakpoint ID and status

**Example Usage:**
```lisp
(repl_set_breakpoint :function "my-function")
(repl_set_breakpoint :function "my-function" :condition "(> x 10)")
```

**Notes:** Requires user approval. Breakpoints can be conditional and limited to specific threads or hit counts.