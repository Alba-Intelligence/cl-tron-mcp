# trace_function

**Short Description:** Trace a function

**Full Description:** Add trace to a function. Every call will print arguments and return value. Use to understand execution flow. Requires approval.

**Parameters:**
- `functionName`: Function name to trace (required)

**Returns:** Trace status

**Example Usage:**
```lisp
(trace_function :functionName "my-function")
```

**Notes:** Requires user approval. Every call to the function will print arguments and return value. Use trace_remove to stop tracing.