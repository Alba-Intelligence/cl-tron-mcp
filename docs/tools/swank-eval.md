# swank_eval

**Short Description:** Evaluate code in SBCL

**Full Description:** Evaluate Lisp code in the connected SBCL session. REQUIRES: swank_connect first. Code runs in a persistent session - state is preserved across calls. Use for testing, debugging, and hot-patching code.

**Parameters:**
- `code`: Lisp code to evaluate (required)
- `package`: Package to evaluate in (optional)

**Returns:** Evaluation result with value, output, and any errors

**Example Usage:**
```lisp
(swank_eval :code "(+ 1 2 3)")
(swank_eval :code "(defun foo (x) (* x 2))" :package "CL-USER")
```

**Notes:** Requires user approval. State is preserved across calls - variables and functions defined in one call are available in subsequent calls.