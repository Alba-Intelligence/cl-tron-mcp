# swank_process_status

**Short Description:** Inspect one Tron-managed SBCL+Swank process

**Full Description:** Return the current status for a single managed process, including whether it is still alive, its uptime, and the communication style it was started with.

**Parameters:**

- `port`: Port of the managed process to inspect (required)

**Returns:** Success status with a `process` plist, or an error if no managed process exists for that port

**Example Usage:**

```lisp
(swank_process_status :port 4010)
```

**Notes:** This is a status lookup only; use `swank_kill` to terminate the process.
