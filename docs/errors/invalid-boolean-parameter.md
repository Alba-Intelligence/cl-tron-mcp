# INVALID_BOOLEAN_PARAMETER

**Error Code:** `INVALID_BOOLEAN_PARAMETER`

**Message:** Invalid boolean parameter: {parameter_name}

## Description

This error occurs when a parameter that should be a boolean (true/false) is not a valid boolean value.

## Common Causes

- Passing a string instead of a boolean
- Passing a number instead of a boolean
- Passing nil when true/false is expected
- Passing an invalid value

## Resolution

1. Ensure the parameter is a boolean (true or false)
2. Use `t` for true and `nil` for false in Lisp
3. Validate before passing

## Example

```lisp
;; Correct - boolean
(trace_function :function-name "PRINT" :enabled t)

;; Incorrect - string
(trace_function :function-name "PRINT" :enabled "true")

;; Incorrect - number
(trace_function :function-name "PRINT" :enabled 1)

;; Validate before calling
(when (or (eq enabled t) (eq enabled nil))
  (trace_function :function-name "PRINT" :enabled enabled))
```

## Related Tools

- `trace_function` - Trace a function
- `breakpoint_set` - Set a breakpoint
- Tools that accept boolean parameters