# repl_describe

**Short Description:** Describe symbol in REPL

**Full Description:** Describe a symbol in the connected REPL. REQUIRES: repl_connect first. Shows documentation, argument list, and type information.

**Parameters:**
- `symbol`: Symbol name to describe (required)

**Returns:** Symbol information including docstring, argument list, and type

**Example Usage:**
```lisp
(repl_describe :symbol "mapcar")
(repl_describe :symbol "cl-tron-mcp/core:start-server")
```

**Notes:** Useful for discovering function signatures and understanding what a symbol does. Shows the symbol's documentation string if available.