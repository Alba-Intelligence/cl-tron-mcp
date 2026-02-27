# inspect_object

**Short Description:** Inspect object by ID

**Full Description:** Inspect an object by its ID. Use when you need to examine the slots and structure of a CLOS instance or other object. Returns type, slots, and nested object IDs for further inspection.

**Parameters:**
- `objectId`: Object ID to inspect (required)
- `maxDepth`: Maximum depth for nested inspection (optional, 0-100)

**Returns:** Object inspection result with type, slots, and nested object IDs

**Example Usage:**
```lisp
(inspect_object :objectId "123" :maxDepth 5)
```

**Notes:** Useful for exploring complex object structures. Nested objects are returned as IDs for further inspection.