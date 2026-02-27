# log_error

**Short Description:** Log error message

**Full Description:** Log an error message. Use for errors that don't crash the program.

**Parameters:**
- `message`: Message to log (required)
- `package`: Package to log for (optional)

**Returns:** Log status

**Example Usage:**
```lisp
(log_error :message "Failed to connect to database")
```

**Notes:** Use for errors that don't crash the program but should be investigated.