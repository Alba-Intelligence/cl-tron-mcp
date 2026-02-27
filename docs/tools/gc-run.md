# gc_run

**Short Description:** Force garbage collection

**Full Description:** Force garbage collection. Use to free memory or test GC behavior. Optionally specify generation (0-6 for SBCL).

**Parameters:**
- `generation`: GC generation to run (optional, 0-6 for SBCL)

**Returns:** GC run status and statistics

**Example Usage:**
```lisp
(gc_run)
(gc_run :generation 2)
```

**Notes:** Useful for freeing memory or testing GC behavior. Different generations control which objects are collected.