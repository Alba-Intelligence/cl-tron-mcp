# swank_threads

**Short Description:** List all SBCL threads

**Full Description:** List all threads in the connected SBCL. REQUIRES: swank_connect first. Shows thread names, status, and IDs for use with other thread tools.

**Parameters:** None

**Returns:** List of thread objects with name, status, and ID

**Example Usage:**
```lisp
(swank_threads)
```

**Notes:** Useful for debugging multi-threaded applications and identifying stuck or deadlocked threads.