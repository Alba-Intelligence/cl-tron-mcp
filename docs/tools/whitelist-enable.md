# whitelist_enable

**Short Description:** Enable/disable whitelist

**Full Description:** Enable or disable the approval whitelist

**Parameters:**
- `enable`: Enable (true) or disable (false) the whitelist (required)

**Returns:** Whitelist enable status

**Example Usage:**
```lisp
(whitelist_enable :enable true)
(whitelist_enable :enable false)
```

**Notes:** When disabled, all operations require approval. When enabled, whitelisted patterns bypass approval.