# INVALID_FRAME_INDEX

**Error Code:** `INVALID_FRAME_INDEX`

**Message:** Invalid frame index: {frame_index}

## Description

This error occurs when a frame index is out of range or not a valid integer. Frame indices must be non-negative integers within the valid range of the current debugger stack.

## Common Causes

- Frame index is negative
- Frame index is greater than the number of frames
- Frame index is not an integer
- No debugger is active

## Resolution

1. Ensure the frame index is a non-negative integer
2. Check the number of available frames first
3. Use `debugger_frames` to get valid frame indices
4. Verify a debugger is active

## Example

```lisp
;; Get available frames
(debugger_frames)
;; => {:frames [{:index 0 :function "foo"} {:index 1 :function "bar"}]}

;; Use valid frame index
(debugger_frame_locals :frame-index 0)

;; Incorrect - out of range
(debugger_frame_locals :frame-index 5)

;; Incorrect - negative
(debugger_frame_locals :frame-index -1)
```

## Related Tools

- `debugger_frames` - Get debugger frames
- `debugger_frame_locals` - Get frame local variables
- `repl_frame_locals` - Get frame local variables