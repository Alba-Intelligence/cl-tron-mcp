# whitelist_clear

**Short Description:** Clear whitelist

**Full Description:** Clear the approval whitelist

**Parameters:**
- `operation`: Operation type to clear (required)

**Returns:** Whitelist clear status

**Example Usage:**
```lisp
(whitelist_clear :operation "eval")
```

**Notes:** Removes all patterns for the specified operation type.