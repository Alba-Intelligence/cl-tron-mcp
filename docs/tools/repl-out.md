# repl_out

**Short Description:** Step out of current frame

**Full Description:** Step out of the current frame. REQUIRES: repl_connect and an active stepping context. Finishes the current function and stops at the return point.

**Parameters:**
- `frame`: Frame index (optional, default: 0)

**Returns:** Current execution state after stepping

**Example Usage:**
```lisp
(repl_out :frame 0)
```

**Notes:** Only works when in an active stepping context. Completes execution of the current function and returns to the caller.