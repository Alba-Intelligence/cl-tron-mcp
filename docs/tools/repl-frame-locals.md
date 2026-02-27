# repl_frame_locals

**Short Description:** Get frame local variables

**Full Description:** Get local variables for a stack frame. REQUIRES: repl_connect and an active debugger session. Use after an error to inspect variable values at the point of failure.

**Parameters:**
- `frame`: Frame index (optional, default: 0)
- `thread`: Thread ID (optional)

**Returns:** Local variables and their values for the specified frame

**Example Usage:**
```lisp
(repl_frame_locals :frame 0)
(repl_frame_locals :frame 1 :thread "main")
```

**Notes:** Only works when in an active debugger session. Useful for inspecting the state of variables at the point where an error occurred.