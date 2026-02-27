# APPROVAL_EXPIRED

**Error Code:** `APPROVAL_EXPIRED`

**Message:** Approval request has expired

## Description

This error occurs when attempting to execute a tool with an approval request that has exceeded the timeout period. Approval requests have a limited lifetime for security reasons.

## Common Causes

- User took too long to approve the request
- Approval request was not processed in time
- System timeout was reached

## Resolution

1. Re-invoke the tool to get a new approval request
2. Approve the new request promptly
3. Check timeout configuration if this happens frequently

## Example

```lisp
;; Original request expired
(repl_eval :code "(+ 1 2)")
;; => Error: APPROVAL_EXPIRED

;; Re-invoke to get new approval request
(repl_eval :code "(+ 1 2)")
;; => Returns new approval request

;; Approve the new request
(approval_respond :request-id "new-req-id" :approved t)

;; Re-invoke with approval
(repl_eval :code "(+ 1 2)" :approval-request-id "new-req-id" :approved t)
```

## Related Tools

- `approval_respond` - Respond to approval request
- `approval_status` - Check approval status
- All tools requiring user approval