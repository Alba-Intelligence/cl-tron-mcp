# log_warn

**Short Description:** Log warning message

**Full Description:** Log a warning message. Use for non-fatal issues that should be noted.

**Parameters:**
- `message`: Message to log (required)
- `package`: Package to log for (optional)

**Returns:** Log status

**Example Usage:**
```lisp
(log_warn :message "Deprecated function called")
```

**Notes:** Use for non-fatal issues that should be noted but don't prevent the program from running.