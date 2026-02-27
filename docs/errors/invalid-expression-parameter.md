# INVALID_EXPRESSION_PARAMETER

**Error Code:** `INVALID_EXPRESSION_PARAMETER`

**Message:** Invalid expression parameter: {parameter_name}

## Description

This error occurs when an expression parameter is not a valid string, is empty, or contains invalid syntax.

## Common Causes

- Expression is not a string
- Expression is empty
- Expression contains invalid Lisp syntax
- Expression is malformed

## Resolution

1. Ensure the expression is a valid string
2. Check that the expression is not empty
3. Verify the expression contains valid Lisp syntax
4. Test the expression in a REPL first

## Example

```lisp
;; Correct - valid expression
(repl_eval :code "(+ 1 2)")

;; Incorrect - empty string
(repl_eval :code "")

;; Incorrect - invalid syntax
(repl_eval :code "(+ 1")

;; Validate before calling
(when (and (stringp code) (> (length code) 0))
  (repl_eval :code code))
```

## Related Tools

- `repl_eval` - Evaluate code
- `swank_eval` - Evaluate code via Swank
- `code_compile_string` - Compile code string