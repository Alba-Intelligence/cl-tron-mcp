# step_frame

**Short Description:** Step execution in frame

**Full Description:** Step execution in a debugger frame. Modes: into (step into calls), over (step over calls), out (step out of current function). Use after hitting a breakpoint.

**Parameters:**
- `frame`: Frame index (optional, default: 0)
- `mode`: Step mode: "into", "over", or "out" (optional)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(step_frame :frame 0 :mode "into")
(step_frame :mode "over")
```

**Notes:** Only works when in an active debugger session. Use after hitting a breakpoint to step through code.