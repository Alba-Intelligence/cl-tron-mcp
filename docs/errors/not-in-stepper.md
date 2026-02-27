# NOT_IN_STEPPER

**Error Code:** `NOT_IN_STEPPER`

**Message:** Not currently in stepper mode

## Description

This error occurs when attempting to use stepper functionality when not in stepper mode. The stepper must be activated first.

## Common Causes

- Attempting to step without activating stepper
- Stepper has been exited
- Calling stepper tools outside of a stepping session

## Resolution

1. Activate stepper mode first
2. Use `swank_step` to start stepping
3. Verify stepper is active before using stepper tools

## Example

```lisp
;; Start stepper mode
(swank_step :action "start")

;; Now can use stepper actions
(swank_step :action "step")
(swank_step :action "next")
(swank_step :action "continue")

;; Exit stepper when done
(swank_step :action "stop")
```

## Related Tools

- `swank_step` - Control stepper
- `repl_step` - Step through code
- `repl_continue` - Continue execution