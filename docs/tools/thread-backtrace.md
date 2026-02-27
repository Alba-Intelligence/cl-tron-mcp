# thread_backtrace

**Short Description:** Get thread backtrace

**Full Description:** Get backtrace for a specific thread. Use to see what a thread is currently doing or where it's blocked.

**Parameters:**
- `threadId`: Thread ID to get backtrace for (required)

**Returns:** Thread backtrace with function calls

**Example Usage:**
```lisp
(thread_backtrace :threadId "main")
```

**Notes:** Use thread_list to get thread IDs. Useful for debugging stuck or blocked threads.