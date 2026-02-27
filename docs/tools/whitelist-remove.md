# whitelist_remove

**Short Description:** Remove whitelist pattern

**Full Description:** Remove a pattern from the approval whitelist

**Parameters:**
- `operation`: Operation type (required)
- `pattern`: Pattern to remove (required)

**Returns:** Whitelist remove status

**Example Usage:**
```lisp
(whitelist_remove :operation "eval" :pattern ".*")
```

**Notes:** Removes a previously added pattern from the whitelist.