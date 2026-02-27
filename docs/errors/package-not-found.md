# PACKAGE_NOT_FOUND

**Error Code:** `PACKAGE_NOT_FOUND`

**Message:** Package not found: {package_name}

## Description

This error occurs when attempting to inspect or access a package that does not exist in the current Lisp image.

## Common Causes

- Package name is invalid or malformed
- Package has not been loaded
- Package name is misspelled
- Package is in a different namespace

## Resolution

1. Verify the package name is correct
2. Check if the package is loaded
3. Load the package if needed
4. List available packages to find correct name

## Example

```lisp
;; Check if package exists
(find-package "CL-USER")
;; => #<PACKAGE "CL-USER">

;; Inspect a package
(inspect_package :package-name "CL-USER")

;; If package not found, list available packages
(do-all-symbols (s)
  (when (symbol-package s)
    (print (package-name (symbol-package s)))))

;; Load a package if needed
(ql:quickload :my-package)
```

## Related Tools

- `inspect_package` - Inspect a package
- `list_packages` - List all packages
- `who_exports` - Find exported symbols