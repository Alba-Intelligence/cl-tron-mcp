# trace_remove

**Short Description:** Remove function trace

**Full Description:** Remove trace from a function. Use when done debugging to stop trace output.

**Parameters:**
- `functionName`: Function name to untrace (required)

**Returns:** Untrace status

**Example Usage:**
```lisp
(trace_remove :functionName "my-function")
```

**Notes:** Requires user approval. Stops tracing output for the specified function. Use trace_list to see all traced functions.