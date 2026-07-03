# swank_send_input

**Short Description:** Answer a pending Swank `read-string` prompt

**Full Description:** Send a string back to the connected Swank session when the debugger or evaluated code is waiting for textual input. This lets an MCP client continue interactive flows that pause on `read-line`, `read`, or similar input requests.

**Parameters:**

- `input`: Input string to send to the pending request (required, may be empty)

**Returns:** Swank's response after the input is forwarded, or an error if no input request is pending

**Example Usage:**

```lisp
(swank_send_input :input "y")
```

**Notes:** This does not create a new evaluation. It only responds to an already-blocked interactive request in the connected Swank session.
