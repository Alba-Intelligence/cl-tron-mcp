# swank_kill

**Short Description:** Terminate a managed SBCL+Swank process

**Full Description:** Stop a process previously created with `swank_launch`. Tron first attempts a graceful shutdown, then forces termination if the process stays alive.

**Parameters:**

- `port`: Port of the managed process to terminate (required)

**Returns:** Success status with the port and PID that were terminated, or an error if no managed process exists for that port

**Example Usage:**

```lisp
(swank_kill :port 4010)
```

**Notes:** Requires approval. This tool only works for processes tracked in Tron's managed-process registry.
