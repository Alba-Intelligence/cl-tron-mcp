# REQUEST_NOT_FOUND

**Error Code:** `REQUEST_NOT_FOUND`

**Message:** Request not found: {request_id}

## Description

This error occurs when attempting to access or manage a request that does not exist in the request registry.

## Common Causes

- Request ID is invalid or malformed
- Request has already been completed and removed
- Request has expired
- Request was never created

## Resolution

1. Verify the request ID is correct
2. Check if the request is still pending
3. Create a new request if needed
4. Use `request_list` to see active requests

## Example

```lisp
;; List active requests
(request_list)

;; If request not found, create a new one
(swank_eval :code "(+ 1 2)")

;; Then use the returned request ID
```

## Related Tools

- `request_list` - List active requests
- `request_cancel` - Cancel a request
- `request_status` - Check request status