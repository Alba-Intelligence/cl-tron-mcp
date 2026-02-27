# repl_eval

**Short Description:** Evaluate Lisp code in REPL

**Full Description:** Evaluate Lisp code in the connected REPL session. REQUIRES: repl_connect first. Code runs in a persistent session - all state (variables, functions, packages) is preserved across calls.

**Parameters:**
- `code`: Lisp code to evaluate (required)
- `package`: Package to evaluate in (optional, default: CL-USER)

**Returns:** Evaluation result with value, output, and any errors

**Example Usage:**
```lisp
(repl_eval :code "(+ 1 2 3)")
(repl_eval :code "(defun foo (x) (* x 2))" :package "CL-USER")
```

**Notes:** Requires user approval. State is preserved across calls - variables and functions defined in one call are available in subsequent calls.