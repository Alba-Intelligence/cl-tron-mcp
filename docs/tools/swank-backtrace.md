# swank_backtrace

**Short Description:** Get call stack

**Full Description:** Get the current call stack from the connected SBCL. REQUIRES: swank_connect first. Use after swank_eval triggers an error to see where it occurred. Shows function names and source locations.

**Parameters:** None

**Returns:** Call stack with function names and source locations

**Example Usage:**
```lisp
(swank_backtrace)
```

**Notes:** Most useful after an error has occurred. Shows the sequence of function calls that led to the current point.