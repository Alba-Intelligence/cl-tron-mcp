# repl_completions

**Short Description:** Get symbol completions

**Full Description:** Get symbol completions in the connected REPL. REQUIRES: repl_connect first. Useful for discovering available functions and variables.

**Parameters:**
- `prefix`: Symbol prefix to complete (required)
- `package`: Package to search in (optional)

**Returns:** List of matching symbols

**Example Usage:**
```lisp
(repl_completions :prefix "mak")
(repl_completions :prefix "map" :package "CL")
```

**Notes:** Useful for discovering available functions and variables. Returns all symbols that start with the given prefix.