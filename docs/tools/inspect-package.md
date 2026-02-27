# inspect_package

**Short Description:** Inspect package symbols

**Full Description:** Inspect a package and list its exported symbols. Use to discover available functions and variables in a package.

**Parameters:**
- `packageName`: Package name to inspect (required)

**Returns:** Package information including exported symbols

**Example Usage:**
```lisp
(inspect_package :packageName "CL-USER")
```

**Notes:** Useful for discovering what functions and variables are available in a package.