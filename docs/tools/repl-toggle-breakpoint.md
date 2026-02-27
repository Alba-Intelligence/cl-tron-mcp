# repl_toggle_breakpoint

**Short Description:** Toggle breakpoint state

**Full Description:** Toggle breakpoint enabled state via the connected REPL

**Parameters:**
- `breakpointId`: Breakpoint ID to toggle (required)

**Returns:** New breakpoint state

**Example Usage:**
```lisp
(repl_toggle_breakpoint :breakpointId 1)
```

**Notes:** Enables or disables a breakpoint without removing it. Use repl_list_breakpoints to see breakpoint IDs.