# PARAMETER_TOO_SMALL

**Error Code:** `PARAMETER_TOO_SMALL`

**Message:** Parameter {parameter_name} is too small (minimum: {min_value})

## Description

This error occurs when a numeric parameter is less than the minimum allowed value.

## Common Causes

- Passing zero when positive is required
- Passing a negative number when non-negative is required
- Passing a number below the minimum threshold

## Resolution

1. Ensure the number meets the minimum value requirement
2. Check that the number is positive (if required)
3. Validate numeric value before passing

## Example

```lisp
;; Correct - meets minimum value
(debugger_frames :frame-index 0)

;; Incorrect - negative number
(debugger_frames :frame-index -1)

;; Validate before calling
(when (and (integerp frame-index) (>= frame-index 0))
  (debugger_frames :frame-index frame-index))
```

## Related Tools

- `debugger_frames` - Get debugger frames
- `repl_frame_locals` - Get frame local variables
- Tools that accept numeric parameters with minimum value requirements