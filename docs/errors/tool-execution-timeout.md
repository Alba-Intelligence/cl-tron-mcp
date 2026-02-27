# TOOL_EXECUTION_TIMEOUT

**Error Code:** `TOOL_EXECUTION_TIMEOUT`

**Message:** Tool execution timed out: {tool_name}

## Description

This error occurs when a tool takes longer to execute than the configured timeout period. This prevents the system from hanging on long-running operations.

## Common Causes

- Code evaluation takes too long
- Infinite loops or blocking operations
- Network operations timing out
- Server is overloaded or unresponsive

## Resolution

1. Increase timeout if operation is expected to take longer
2. Break long operations into smaller chunks
3. Check for infinite loops or blocking operations
4. Verify server is responsive

## Example

```lisp
;; For long operations, break them down
;; Instead of: (swank_eval :code "(dotimes (i 1000000) (print i))")
;; Use: (swank_eval :code "(print 1000000)")

;; Or use profiling to identify bottlenecks
(profile_start)
(swank_eval :code "(my-function)")
(profile_stop)
```

## Related Tools

- `profile_start` - Start profiling
- `profile_stop` - Stop profiling
- All tools that may have long execution times