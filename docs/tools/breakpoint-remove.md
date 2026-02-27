# breakpoint_remove

**Short Description:** Remove a breakpoint

**Full Description:** Remove a breakpoint by its ID. Use to clear breakpoints after debugging is complete.

**Parameters:**
- `breakpointId`: Breakpoint ID to remove (required)

**Returns:** Removal status

**Example Usage:**
```lisp
(breakpoint_remove :breakpointId 1)
```

**Notes:** Use breakpoint_list to see all breakpoints and their IDs.