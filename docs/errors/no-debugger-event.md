# NO_DEBUGGER_EVENT

**Error Code:** `NO_DEBUGGER_EVENT`

**Message:** No debugger event is active

## Description

This error occurs when attempting to access debugger functionality when no debugger event (breakpoint, error, or interrupt) is currently active.

## Common Causes

- Attempting to inspect frames when not in debugger
- Calling debugger tools outside of a debugging session
- Debugger has already been exited

## Resolution

1. Trigger a debugger event (breakpoint, error, or interrupt)
2. Use `debugger_status` to check if debugger is active
3. Set a breakpoint and wait for it to be hit
4. Interrupt execution to enter debugger

## Example

```lisp
;; Set a breakpoint
(breakpoint_set :function-name "my-function")

;; Trigger the function to hit breakpoint
(swank_eval :code "(my-function)")

;; Now debugger is active, can inspect frames
(debugger_frames)

;; Or interrupt execution
(swank_interrupt)
```

## Related Tools

- `debugger_status` - Check debugger status
- `debugger_frames` - Get debugger frames
- `breakpoint_set` - Set a breakpoint
- `swank_interrupt` - Interrupt execution