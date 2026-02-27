# repl_doc

**Short Description:** Get symbol documentation

**Full Description:** Get documentation for a symbol in the connected REPL. REQUIRES: repl_connect first. Returns docstring and argument list.

**Parameters:**
- `symbol`: Symbol name to get documentation for (required)

**Returns:** Documentation string and argument list

**Example Usage:**
```lisp
(repl_doc :symbol "mapcar")
```

**Notes:** Returns the docstring for the symbol if available, along with its argument list. Useful for understanding how to use a function.