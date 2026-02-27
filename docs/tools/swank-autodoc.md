# swank_autodoc

**Short Description:** Get argument list and docs

**Full Description:** Get argument list and documentation for a symbol. REQUIRES: swank_connect first. Example: 'mapcar' shows (function list &rest more-lists).

**Parameters:**
- `symbol`: Symbol name to get autodoc for (required)

**Returns:** Argument list and documentation

**Example Usage:**
```lisp
(swank_autodoc :symbol "mapcar")
```

**Notes:** Returns the function's argument list and documentation string. Useful for understanding function signatures.