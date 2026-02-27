# repl_eval

**Short Description:** Evaluate code in REPL

**Full Description:** Evaluate Lisp code in REPL context. Use for testing, debugging, and modifying running code. Requires connection to Swank. Code runs in the persistent Lisp session.

**Parameters:**
- `code`: Lisp code to evaluate (required)
- `package`: Package to evaluate in (optional)

**Returns:** Evaluation result with value, output, and any errors

**Example Usage:**
```lisp
(repl_eval :code "(+ 1 2 3)")
(repl_eval :code "(defun foo (x) (* x 2))" :package "CL-USER")
```

**Notes:** Requires user approval. State is preserved across calls - variables and functions defined in one call are available in subsequent calls.