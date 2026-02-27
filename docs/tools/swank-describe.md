# swank_describe

**Short Description:** Describe symbol in SBCL

**Full Description:** Describe a symbol in the connected SBCL. REQUIRES: swank_connect first. Shows documentation, argument list, and source location.

**Parameters:**
- `expression`: Symbol expression to describe (required)

**Returns:** Symbol information including docstring, argument list, and source location

**Example Usage:**
```lisp
(swank_describe :expression "'mapcar")
```

**Notes:** Useful for discovering function signatures and understanding what a symbol does. Shows the symbol's documentation string if available.