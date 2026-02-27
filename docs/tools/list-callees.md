# list_callees

**Short Description:** List called functions

**Full Description:** List functions called by the given symbol

**Parameters:**
- `symbolName`: Symbol name to find callees for (required)

**Returns:** List of functions called by the symbol

**Example Usage:**
```lisp
(list_callees :symbolName "my-function")
```

**Notes:** Useful for understanding what functions a given function calls.