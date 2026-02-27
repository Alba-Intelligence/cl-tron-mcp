# swank_inspect

**Short Description:** Inspect object in SBCL

**Full Description:** Inspect an object in the connected SBCL. REQUIRES: swank_connect first. EXPRESSION is evaluated, so use quoted symbols for variables: '*my-var*' or expressions: '(make-hash-table)'.

**Parameters:**
- `expression`: Lisp expression to inspect (required)

**Returns:** Object inspection result with type, value, and slots

**Example Usage:**
```lisp
(swank_inspect :expression "'*my-var*")
(swank_inspect :expression "'(make-hash-table)")
```

**Notes:** The expression is evaluated, so use quoted symbols for variables. Returns detailed information about the object.