# INVALID_STRING_PARAMETER

**Error Code:** `INVALID_STRING_PARAMETER`

**Message:** Invalid string parameter: {parameter_name}

## Description

This error occurs when a parameter that should be a string is not a string, is empty, or contains invalid characters.

## Common Causes

- Passing a number instead of a string
- Passing nil instead of a string
- Passing an empty string when non-empty is required
- Passing a list or other type instead of a string

## Resolution

1. Ensure the parameter is a string type
2. Check that the string is not empty (if required)
3. Verify the string contains valid characters
4. Use `stringp` to validate before passing

## Example

```lisp
;; Correct
(repl_eval :code "(+ 1 2)")

;; Incorrect - number instead of string
(repl_eval :code 123)

;; Incorrect - nil instead of string
(repl_eval :code nil)

;; Validate before calling
(when (and code (stringp code))
  (repl_eval :code code))
```

## Related Tools

- All tools that accept string parameters
- `repl_eval`, `swank_eval`, `code_compile_string`