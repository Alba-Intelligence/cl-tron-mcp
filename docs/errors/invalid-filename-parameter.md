# INVALID_FILENAME_PARAMETER

**Error Code:** `INVALID_FILENAME_PARAMETER`

**Message:** Invalid filename parameter: {parameter_name}

## Description

This error occurs when a filename parameter is not a valid string, is empty, or contains invalid characters.

## Common Causes

- Filename is not a string
- Filename is empty
- Filename contains invalid characters for the filesystem
- Path does not exist (when required)

## Resolution

1. Ensure the filename is a valid string
2. Check that the filename is not empty
3. Verify the filename contains valid characters
4. Check that the path exists (if required)

## Example

```lisp
;; Correct - valid filename
(code_compile_file :filename "/path/to/file.lisp")

;; Incorrect - empty string
(code_compile_file :filename "")

;; Incorrect - not a string
(code_compile_file :filename 123)

;; Validate before calling
(when (and (stringp filename) (> (length filename) 0))
  (code_compile_file :filename filename))
```

## Related Tools

- `code_compile_file` - Compile a file
- `reload_system` - Reload a system (accepts system name, not filename)