# INVALID_INTEGER_PARAMETER

**Error Code:** `INVALID_INTEGER_PARAMETER`

**Message:** Invalid integer parameter: {parameter_name}

## Description

This error occurs when a parameter that should be an integer is not an integer, is negative (when positive is required), or is outside the valid range.

## Common Causes

- Passing a string instead of an integer
- Passing a float instead of an integer
- Passing nil instead of an integer
- Passing a negative number when positive is required
- Passing a number outside the valid range

## Resolution

1. Ensure the parameter is an integer type
2. Check that the integer is within the valid range
3. Verify the integer is positive (if required)
4. Use `integerp` to validate before passing

## Example

```lisp
;; Correct - positive integer
(debugger_frames :frame-index 0)

;; Incorrect - string instead of integer
(debugger_frames :frame-index "0")

;; Incorrect - negative number
(debugger_frames :frame-index -1)

;; Validate before calling
(when (and (integerp frame-index) (>= frame-index 0))
  (debugger_frames :frame-index frame-index))
```

## Related Tools

- `debugger_frames` - Get debugger frames
- `repl_frame_locals` - Get frame local variables
- Tools that accept integer parameters (frame indices, ports, etc.)