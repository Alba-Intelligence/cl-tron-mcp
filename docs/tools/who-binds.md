# who_binds

**Short Description:** Find symbol bindings

**Full Description:** Find bindings of the given symbol (let bindings, function parameters). Use to see where a variable is bound.

**Parameters:**
- `symbolName`: Symbol name to find bindings for (required)

**Returns:** List of locations where the symbol is bound

**Example Usage:**
```lisp
(who_binds :symbolName "x")
```

**Notes:** Useful for finding all places where a variable is bound via let, function parameters, etc.