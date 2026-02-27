# INVALID_SYMBOL_PARAMETER

**Error Code:** `INVALID_SYMBOL_PARAMETER`

**Message:** Invalid symbol parameter: {parameter_name}

## Description

This error occurs when a symbol parameter is not a valid Common Lisp symbol name or the symbol does not exist.

## Common Causes

- Symbol name is not a string
- Symbol does not exist in the current package
- Symbol name contains invalid characters
- Symbol name is empty

## Resolution

1. Ensure the symbol name is a valid string
2. Verify the symbol exists in the current package
3. Use `find-symbol` to check symbol existence
4. Specify the package if needed (e.g., "CL-USER:MY-FUNCTION")

## Example

```lisp
;; Correct - existing symbol
(inspect_function :function-name "PRINT")

;; Incorrect - symbol doesn't exist
(inspect_function :function-name "NON-EXISTENT-FUNCTION")

;; Check if symbol exists
(find-symbol "PRINT" "COMMON-LISP")
;; => PRINT, :EXTERNAL

;; Specify package
(inspect_function :function-name "CL-USER:MY-FUNCTION")
```

## Related Tools

- `inspect_function` - Inspect a function
- `trace_function` - Trace a function
- `who_calls` - Find callers of a function