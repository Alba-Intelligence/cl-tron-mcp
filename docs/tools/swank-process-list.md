# swank_process_list

**Short Description:** List Tron-managed SBCL+Swank processes

**Full Description:** Return the current contents of Tron's managed-process registry, including port, PID, host, uptime, and communication style for each launched process.

**Parameters:** None

**Returns:** A success plist containing `count` and `processes`

**Example Usage:**

```lisp
(swank_process_list)
```

**Notes:** Only processes launched through `swank_launch` appear here.
