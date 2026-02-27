# who_calls

**Short Description:** Find function callers

**Full Description:** Find functions that call the given symbol. Use to understand dependencies before modifying a function.

**Parameters:**
- `symbolName`: Symbol name to find callers for (required)

**Returns:** List of functions that call the symbol

**Example Usage:**
```lisp
(who_calls :symbolName "my-function")
```

**Notes:** Useful for understanding dependencies and impact analysis before modifying a function.