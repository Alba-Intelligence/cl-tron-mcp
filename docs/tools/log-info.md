# log_info

**Short Description:** Log info message

**Full Description:** Log an info message. Use for general information about program execution.

**Parameters:**
- `message`: Message to log (required)
- `package`: Package to log for (optional)

**Returns:** Log status

**Example Usage:**
```lisp
(log_info :message "Server started")
(log_info :message "Processing request" :package "MY-APP")
```

**Notes:** Use for general informational messages about program execution.