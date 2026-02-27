# repl_backtrace

**Short Description:** Get REPL call stack

**Full Description:** Get the call stack from the connected REPL. REQUIRES: repl_connect first. Use after an error to see where it occurred in your code.

**Parameters:** None

**Returns:** Call stack with function names, arguments, and source locations

**Example Usage:**
```lisp
(repl_backtrace)
```

**Notes:** Most useful after an error has occurred. Shows the sequence of function calls that led to the current point.