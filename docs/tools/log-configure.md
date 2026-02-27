# log_configure

**Short Description:** Configure logging level

**Full Description:** Configure logging level for a package. Levels: trace, debug, info, warn, error, fatal. Use to control log verbosity.

**Parameters:**
- `level`: Log level: "trace", "debug", "info", "warn", "error", or "fatal" (optional)
- `package`: Package to configure (optional)
- `appender`: Appender to configure (optional)

**Returns:** Configuration status

**Example Usage:**
```lisp
(log_configure :level "debug" :package "CL-USER")
```

**Notes:** Use to control log verbosity for specific packages or globally.