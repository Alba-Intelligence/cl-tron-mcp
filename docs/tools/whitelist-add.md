# whitelist_add

**Short Description:** Add whitelist pattern

**Full Description:** Add a pattern to the approval whitelist

**Parameters:**
- `operation`: Operation type to whitelist (required)
- `pattern`: Pattern to match (required)

**Returns:** Whitelist add status

**Example Usage:**
```lisp
(whitelist_add :operation "eval" :pattern ".*")
```

**Notes:** Patterns are matched against operation details. Use to allow specific operations without approval.