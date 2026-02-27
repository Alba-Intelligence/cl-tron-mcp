# repl_compile

**Short Description:** Compile and load Lisp code

**Full Description:** Compile and load Lisp code in the connected REPL. REQUIRES: repl_connect first. Use for hot-reloading function definitions without restarting the session.

**Parameters:**
- `code`: Lisp code to compile (required)
- `package`: Package to compile in (optional)
- `filename`: Filename for debugging messages (optional)

**Returns:** Compilation result with status and any warnings/errors

**Example Usage:**
```lisp
(repl_compile :code "(defun bar (x) (+ x 1))" :filename "mycode.lisp")
```

**Notes:** Requires user approval. Useful for hot-reloading code during development without restarting the SBCL session.