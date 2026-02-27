# repl_threads

**Short Description:** List all REPL threads

**Full Description:** List all threads in the connected REPL. REQUIRES: repl_connect first. Shows thread names, status, and IDs for debugging concurrency issues.

**Parameters:** None

**Returns:** List of thread objects with name, status, and ID

**Example Usage:**
```lisp
(repl_threads)
```

**Notes:** Useful for debugging multi-threaded applications and identifying stuck or deadlocked threads.