# INVALID_LIST_PARAMETER

**Error Code:** `INVALID_LIST_PARAMETER`

**Message:** Invalid list parameter: {parameter_name}

## Description

This error occurs when a parameter that should be a list is not a list, or is not in the expected format.

## Common Causes

- Passing a string instead of a list
- Passing nil instead of a list (when list is required)
- Passing a number or other type instead of a list
- Passing a list with incorrect structure

## Resolution

1. Ensure the parameter is a list type
2. Check that the list has the expected structure
3. Verify list elements are of correct types
4. Use `listp` to validate before passing

## Example

```lisp
;; Correct - passing a list
(inspect_object :object-id '(1 2 3))

;; Incorrect - string instead of list
(inspect_object :object-id "(1 2 3)")

;; Incorrect - nil instead of list
(inspect_object :object-id nil)

;; Validate before calling
(when (listp object-id)
  (inspect_object :object-id object-id))
```

## Related Tools

- `inspect_object` - Inspect an object
- `trace_function` - Trace functions (accepts list of function names)
- Tools that accept list parameters