# swank_completions

**Short Description:** Get symbol completions

**Full Description:** Get symbol completions in the connected SBCL. REQUIRES: swank_connect first. Example: prefix 'mak' returns (make-array make-hash-table make-instance ...).

**Parameters:**
- `symbol`: Symbol prefix to complete (required)
- `package`: Package to search in (optional)

**Returns:** List of matching symbols

**Example Usage:**
```lisp
(swank_completions :symbol "mak")
(swank_completions :symbol "map" :package "CL")
```

**Notes:** Useful for discovering available functions and variables. Returns all symbols that start with the given prefix.