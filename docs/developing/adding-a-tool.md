# Adding a tool

1. **Implement** the behaviour in the owning package (`src/inspector/`, `src/debugger/`,
   `src/swank/`, ...). Return a plist; signal nothing for expected failures, return
   `(list :error t :message ...)` or an error from `src/resources/error-codes.lisp`.
2. **Register** it in the matching `src/tools/*-tools.lisp` with `define-validated-tool`
   (`src/tools/macros.lisp`):

```lisp
(define-validated-tool "inspect_class"
  "Inspect a CLOS class: slots, superclasses, and methods"
  :input-schema (list :className "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/inspect-class.md"
  :validation ((validate-string "class_name" class_name :required t :min-length 1))
  :body (cl-tron-mcp/inspector:inspect-class :class-name class_name))
```

   - The first sentence of the description is what the generated catalogue shows. Make it a clear
     one-liner.
   - **Parameter naming:** the Lisp reader upcases the keys in `:input-schema`, so camelCase is lost
     (`:className` is read as `CLASSNAME`) and the registry serves lowercase property names. The handler
     lambda binds snake_case variables named in `:validation`; incoming keys are normalised to match.
     Declare every parameter in `:validation` (even if only `(when x (validate-... ))`) so it is bound.
   - Set `:requires-approval t` for anything that evaluates code, changes the image, starts or stops
     processes, or alters tracing/profiling/breakpoints. If it needs a whitelist operation mapping, add
     it to `*tool-name-to-operation*` in `src/security/approval.lisp`.
3. **Add a new file** to the `:cl-tron-mcp/tools` component list in `cl-tron-mcp.asd`, in order.
4. **Test:** add a Rove test under `tests/` (see `tests/validation-test.lisp`, `tests/mcp-e2e-test.lisp`).
   Test the failure path, not only the happy path.
5. **Document:** write `docs/tools/<tool-name-with-dashes>.md` (parameters, returns, an example), then run
   `sbcl --noinform --non-interactive --load scripts/gen-docs.lisp` and commit the regenerated
   `docs/using/tools.md`. Tests fail if you forget either step.
