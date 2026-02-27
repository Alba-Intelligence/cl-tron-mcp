# breakpoint_set

**Short Description:** Set a breakpoint

**Full Description:** Set a breakpoint on a function. Execution will pause when the function is called. Requires approval. Use for proactive debugging.

**Parameters:**
- `functionName`: Function name to set breakpoint on (required)
- `condition`: Conditional expression (optional)
- `hitCount`: Number of hits before triggering (optional)

**Returns:** Breakpoint ID and status

**Example Usage:**
```lisp
(breakpoint_set :functionName "my-function")
(breakpoint_set :functionName "my-function" :condition "(> x 10)")
```

**Notes:** Requires user approval. Breakpoints can be conditional and limited to specific hit counts.