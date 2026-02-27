# repl_inspect

**Short Description:** Inspect object in REPL

**Full Description:** Inspect an object in the connected REPL. REQUIRES: repl_connect first. EXPRESSION is evaluated - use quoted symbols for variables.

**Parameters:**
- `expression`: Lisp expression to inspect (required)

**Returns:** Object inspection result with type, value, and slots

**Example Usage:**
```lisp
(repl_inspect :expression "'*my-var*")
(repl_inspect :expression "'(make-hash-table)")
```

**Notes:** The expression is evaluated, so use quoted symbols for variables. Returns detailed information about the object including its type, value, and internal structure.