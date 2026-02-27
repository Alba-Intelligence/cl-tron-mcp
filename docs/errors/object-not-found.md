# OBJECT_NOT_FOUND

**Error Code:** `OBJECT_NOT_FOUND`

**Message:** Object not found: {object_id}

## Description

This error occurs when attempting to inspect or access an object that does not exist or is not accessible.

## Common Causes

- Object ID is invalid or malformed
- Object has been garbage collected
- Object does not exist in the current context
- Object is not accessible from current scope

## Resolution

1. Verify the object ID is correct
2. Check if the object still exists
3. Ensure the object is in scope
4. Use `inspect_object` with a valid object

## Example

```lisp
;; Create an object
(defparameter *my-object* (list 1 2 3))

;; Inspect it
(inspect_object :object-id "*my-object*")

;; If object not found, check if it exists
(boundp '*my-object*)
;; => T

;; List available objects
(apropos-list "")
```

## Related Tools

- `inspect_object` - Inspect an object
- `apropos` - Find symbols by prefix
- `list_symbols` - List symbols