# inspect_class

**Short Description:** Inspect CLOS class

**Full Description:** Inspect a CLOS class definition. Shows superclasses, slots, and methods. Use to understand class structure before working with instances.

**Parameters:**
- `className`: Class name to inspect (required)

**Returns:** Class information including superclasses, slots, and methods

**Example Usage:**
```lisp
(inspect_class :className "my-class")
```

**Notes:** Useful for understanding the structure of CLOS classes and their inheritance hierarchy.