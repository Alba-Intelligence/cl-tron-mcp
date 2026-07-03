# breakpoint_toggle

**Short Description:** Enable or disable a breakpoint by ID

**Full Description:** Toggle an existing breakpoint without deleting it. This is useful when you want to temporarily suppress a breakpoint while preserving its ID and configuration.

**Parameters:**

- `breakpointId`: Numeric breakpoint identifier (required)

**Returns:** Breakpoint status after the toggle operation, or an error if the breakpoint does not exist

**Example Usage:**

```lisp
(breakpoint_toggle :breakpointId 3)
```

**Notes:** Requires approval because it changes debugger behavior in the running Lisp image.
