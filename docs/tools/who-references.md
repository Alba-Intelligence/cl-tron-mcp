# who_references

**Short Description:** Find symbol references

**Full Description:** Find references to the given symbol (variable references). Use to see where a variable is used.

**Parameters:**
- `symbolName`: Symbol name to find references for (required)

**Returns:** List of locations where the symbol is referenced

**Example Usage:**
```lisp
(who_references :symbolName "*my-var*")
```

**Notes:** Useful for finding all places where a variable is referenced.