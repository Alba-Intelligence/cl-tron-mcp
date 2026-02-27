# repl_remove_breakpoint

**Short Description:** Remove a breakpoint

**Full Description:** Remove a breakpoint by ID via the connected REPL

**Parameters:**
- `breakpointId`: Breakpoint ID to remove (required)

**Returns:** Removal status

**Example Usage:**
```lisp
(repl_remove_breakpoint :breakpointId 1)
```

**Notes:** Use repl_list_breakpoints to see all breakpoints and their IDs.