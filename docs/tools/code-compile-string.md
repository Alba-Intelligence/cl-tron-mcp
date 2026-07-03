# code_compile_string

**Short Description:** Compile and load code string

**Full Description:** Compile and load Lisp code into the running image. When Tron is connected to Swank it uses the remote compile path; otherwise it falls back to local compilation inside the MCP process. Multi-form code strings are supported. Requires approval. Code persists only in memory - update source files separately.

**Parameters:**

- `code`: Lisp code to compile (required)
- `filename`: Filename for debugging messages (optional)
- `package`: Package to compile in (optional, defaults to `CL-USER`)

**Returns:** Compilation result with status and any warnings/errors

**Example Usage:**

```lisp
(code_compile_string
  :code "(defun bar (x) (+ x 1))"
  :filename "mycode.lisp"
  :package "CL-USER")
```

**Notes:** Requires user approval. Code is loaded into memory only - not persisted to disk. Update source files separately for permanent changes.
