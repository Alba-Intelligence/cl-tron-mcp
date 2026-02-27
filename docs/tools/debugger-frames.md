# debugger_frames

**Short Description:** Get debugger stack frames

**Full Description:** Get debugger stack frames when an error has occurred. Use after swank_eval returns an error to see the call stack. Shows function names and source locations.

**Parameters:** None

**Returns:** Stack frames with function names and source locations

**Example Usage:**
```lisp
(debugger_frames)
```

**Notes:** Most useful after an error has occurred. Shows the sequence of function calls that led to the error.