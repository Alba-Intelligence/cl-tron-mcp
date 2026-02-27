# PARAMETER_TOO_SHORT

**Error Code:** `PARAMETER_TOO_SHORT`

**Message:** Parameter {parameter_name} is too short (minimum: {min_length} characters)

## Description

This error occurs when a string parameter is shorter than the minimum required length.

## Common Causes

- Passing an empty string when non-empty is required
- Passing a string that doesn't meet minimum length requirements
- Truncating strings before passing them

## Resolution

1. Ensure the string meets the minimum length requirement
2. Check the string is not empty
3. Validate string length before passing

## Example

```lisp
;; Correct - meets minimum length
(repl_eval :code "(+ 1 2)")

;; Incorrect - empty string
(repl_eval :code "")

;; Validate before calling
(when (and (stringp code) (>= (length code) 1))
  (repl_eval :code code))
```

## Related Tools

- `repl_eval` - Evaluate code
- `swank_eval` - Evaluate code via Swank
- `code_compile_string` - Compile code string
- Tools that accept string parameters with minimum length requirements