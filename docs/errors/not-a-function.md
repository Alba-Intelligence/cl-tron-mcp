# NOT_A_FUNCTION

**Error Code:** `NOT_A_FUNCTION`

**Message:** Not a function: {name}

## Description

This error occurs when attempting to inspect, trace, or call something that is not a function.

## Common Causes

- The name refers to a variable, not a function
- The name refers to a macro or special operator
- The function does not exist
- The name is not a valid symbol

## Resolution

1. Verify the name refers to a function
2. Use `functionp` to check if it's a function
3. Check if it's a macro or special operator
4. Use `inspect_function` to verify

## Example

```lisp
;; Check if it's a function
(functionp #'print)
;; => T

;; Inspect to verify
(inspect_function :function-name "PRINT")

;; If it's a macro, use appropriate tools
(inspect_macro :macro-name "WHEN")
```

## Related Tools

- `inspect_function` - Inspect a function
- `trace_function` - Trace a function
- `who_calls` - Find callers of a function