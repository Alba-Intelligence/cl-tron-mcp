# swank_abort

**Short Description:** Abort a thread

**Full Description:** Abort a specific thread in the connected SBCL. REQUIRES: swank_connect first. Useful when a thread is stuck or in an error condition.

**Parameters:**
- `threadId`: Thread ID to abort (required)

**Returns:** Abort status

**Example Usage:**
```lisp
(swank_abort :threadId "main")
```

**Notes:** Requires user approval. Use swank_threads to get thread IDs. Aborting a thread terminates it forcefully.