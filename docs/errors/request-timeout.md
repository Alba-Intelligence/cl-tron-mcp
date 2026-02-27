# REQUEST_TIMEOUT

**Error Code:** `REQUEST_TIMEOUT`

**Message:** Request timed out: {request_id}

## Description

This error occurs when a request does not complete within the expected time frame. This can happen due to long-running operations, network issues, or server problems.

## Common Causes

- Code evaluation takes too long
- Network connectivity issues
- Server is overloaded or unresponsive
- Infinite loops or blocking operations

## Resolution

1. Increase timeout if operation is expected to take longer
2. Check server status and resources
3. Break long operations into smaller chunks
4. Use `request_cancel` to cancel and retry

## Example

```lisp
;; Cancel timed out request
(request_cancel :request-id "req-123")

;; Retry with simpler operation
(swank_eval :code "(+ 1 2)")

;; For long operations, consider breaking them down
;; Instead of: (swank_eval :code "(dotimes (i 1000000) (print i))")
;; Use: (swank_eval :code "(print 1000000)")
```

## Related Tools

- `request_cancel` - Cancel a request
- `request_status` - Check request status
- `request_list` - List active requests