# thread_inspect

**Short Description:** Inspect thread details

**Full Description:** Get detailed information about a specific thread including name, state, and stack usage. Use to understand thread behavior.

**Parameters:**
- `threadId`: Thread ID to inspect (required)

**Returns:** Thread information including name, state, and stack usage

**Example Usage:**
```lisp
(thread_inspect :threadId "main")
```

**Notes:** Use thread_list to get thread IDs. Useful for understanding what a thread is doing and its resource usage.