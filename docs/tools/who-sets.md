# who_sets

**Short Description:** Find symbol modifications

**Full Description:** Find setq/makunbound of the given symbol. Use to see where a variable is modified.

**Parameters:**
- `symbolName`: Symbol name to find modifications for (required)

**Returns:** List of locations where the symbol is modified

**Example Usage:**
```lisp
(who_sets :symbolName "*my-var*")
```

**Notes:** Useful for finding all places where a variable is modified via setq or makunbound.