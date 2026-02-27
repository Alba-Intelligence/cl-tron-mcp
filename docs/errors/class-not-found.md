# CLASS_NOT_FOUND

**Error Code:** `CLASS_NOT_FOUND`

**Message:** Class not found: {class_name}

## Description

This error occurs when attempting to inspect or access a class that does not exist in the current Lisp image.

## Common Causes

- Class name is invalid or malformed
- Class has not been defined
- Class is in a different package
- Class name is misspelled

## Resolution

1. Verify the class name is correct
2. Check if the class is defined
3. Specify the package if needed
4. List available classes to find correct name

## Example

```lisp
;; Check if class exists
(find-class 'standard-object nil)
;; => #<STANDARD-CLASS STANDARD-CLASS>

;; Inspect a class
(inspect_class :class-name "STANDARD-OBJECT")

;; If class not found, list available classes
;; In SBCL:
(sb-mop:class-direct-subclasses (find-class 'standard-object))
```

## Related Tools

- `inspect_class` - Inspect a class
- `list_classes` - List classes
- `who_specializes` - Find specializing classes