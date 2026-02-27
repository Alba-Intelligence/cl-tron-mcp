# INVALID_PACKAGE_PARAMETER

**Error Code:** `INVALID_PACKAGE_PARAMETER`

**Message:** Invalid package parameter: {parameter_name}

## Description

This error occurs when a package parameter is not a valid Common Lisp package name or the package does not exist.

## Common Causes

- Package name is not a string
- Package does not exist in the Lisp image
- Package name contains invalid characters
- Package name is empty

## Resolution

1. Ensure the package name is a valid string
2. Verify the package exists in the Lisp image
3. Use `find-package` to check package existence
4. List available packages to find correct name

## Example

```lisp
;; Correct - existing package
(inspect_package :package-name "COMMON-LISP")

;; Incorrect - package doesn't exist
(inspect_package :package-name "NON-EXISTENT-PACKAGE")

;; Check if package exists
(find-package "CL-USER")
;; => #<PACKAGE "CL-USER">

;; List all packages
(apropos-list "" :package t)
```

## Related Tools

- `inspect_package` - Inspect a package
- `list_packages` - List all packages
- `who_calls` - Find callers (accepts package parameter)