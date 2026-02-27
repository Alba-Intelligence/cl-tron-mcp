# INVALID_PREFIX_PARAMETER

**Error Code:** `INVALID_PREFIX_PARAMETER`

**Message:** Invalid prefix parameter: {parameter_name}

## Description

This error occurs when a prefix parameter is not a valid string, is empty, or contains invalid characters. Prefixes are used for filtering or matching operations.

## Common Causes

- Prefix is not a string
- Prefix is empty
- Prefix contains invalid characters
- Prefix is too long

## Resolution

1. Ensure the prefix is a valid string
2. Check that the prefix is not empty
3. Verify the prefix contains valid characters
4. Keep prefix length reasonable

## Example

```lisp
;; Correct - valid prefix
(apropos :prefix "print")

;; Incorrect - empty string
(apropos :prefix "")

;; Incorrect - not a string
(apropos :prefix 123)

;; Validate before calling
(when (and (stringp prefix) (> (length prefix) 0))
  (apropos :prefix prefix))
```

## Related Tools

- `apropos` - Find symbols by prefix
- `list_symbols` - List symbols (accepts prefix filter)