# inspect_function

**Short Description:** Inspect function definition

**Full Description:** Inspect a function definition. Shows lambda list, documentation, and source location. Use to understand how a function should be called.

**Parameters:**
- `symbolName`: Function symbol name (required)

**Returns:** Function information including lambda list, documentation, and source location

**Example Usage:**
```lisp
(inspect_function :symbolName "mapcar")
```

**Notes:** Useful for discovering function signatures and understanding what parameters a function accepts.