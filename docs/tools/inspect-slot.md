# inspect_slot

**Short Description:** Get or set slot value

**Full Description:** Get or set a slot value on an object. Use to read or modify individual slots of a CLOS instance. Set value by providing the value parameter.

**Parameters:**
- `objectId`: Object ID (required)
- `slotName`: Slot name (required)
- `value`: New value to set (optional)

**Returns:** Current or updated slot value

**Example Usage:**
```lisp
(inspect_slot :objectId "123" :slotName "name")
(inspect_slot :objectId "123" :slotName "name" :value "new-value")
```

**Notes:** Without value parameter, returns current value. With value parameter, sets the slot to the new value.