# Hot Code Reloading Development

This guide covers hot code reloading workflows for modifying Lisp code without restarting the application.

## Quick Reference

**Hot Reload Tool Cheat Sheet:**
| Task | Tool | Approval Required |
|------|------|------------------|
| Compile and load string | `code_compile_string` | Yes (:compile-file) |
| Load FASL file | `code_load_fasl` | Yes (:modify-running-code) |
| Reload package | `code_reload_package` | Yes (:modify-running-code) |
| Reload ASDF system | `code_reload_system` | Yes (:modify-running-code) |
| Atomic function replace | `code_replace_function` | Yes (:modify-running-code) |
| Get source location | `source_location` | No |

## Core Philosophy

Common Lisp's hot reloading is unique:
- **Atomic Replacement**: Function definitions swap atomically
- **Running Threads Continue**: Threads using old code finish with old code
- **New Calls Get New Code**: Immediately after replacement
- **No Full Restart Needed**: Just load new code

## Workflow 1: Safe Function Replacement

### Step 1: Locate the Function

```json
{
  "tool": "source_location",
  "arguments": {
    "symbol": "my-app:compute-result"
  }
}
```

Returns file, line, and column.

### Step 2: Read Current Definition

```json
{
  "tool": "lisp-read-file",
  "arguments": {
    "path": "src/compute.lisp",
    "collapsed": true
  }
}
```

### Step 3: Modify Code

```json
{
  "tool": "lisp-edit-form",
  "arguments": {
    "operation": "replace",
    "file_path": "src/compute.lisp",
    "form_type": "defun",
    "form_name": "compute-result",
    "content": "(defun compute-result (x y)\n  (+ x y))\n"
  }
}
```

### Step 4: Compile and Load

```json
{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(defun compute-result (x y) (+ x y))",
    "filename": "src/compute.lisp"
  }
}
```

### Step 5: Verify

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:compute-result 1 2)",
    "package": "MY-APP"
  }
}
```

## Workflow 2: Package Reload

Reload entire package when multiple functions change.

### Step 1: List Package Files

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(asdf:system-source-files :my-app)",
    "package": "CL-USER"
  }
}
```

### Step 2: Reload Package

```json
{
  "tool": "code_reload_package",
  "arguments": {
    "package": "MY-APP"
  }
}
```

This recompiles and reloads all files in the package.

### Step 3: Verify All Functions

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:run-tests)",
    "package": "MY-APP"
  }
}
```

## Workflow 3: Full System Reload

Reload complete ASDF system with dependencies.

### Step 1: Check System Status

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(asdf:system-relative-pathname :my-app \"\")",
    "package": "CL-USER"
  }
}
```

### Step 2: Reload System

```json
{
  "tool": "code_reload_system",
  "arguments": {
    "system": "my-app",
    "force": true
  }
}
```

The `force: true` recompiles all files.

### Step 3: Verify System Health

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(and (fboundp 'my-app:main)\n          (my-app:health-check))",
    "package": "MY-APP"
  }
}
```

## Workflow 4: Hot Patch Emergency Fix

Critical fix applied immediately.

### Step 1: Create Minimal Fix

```json
{
  "tool": "code_compile_string",
  "arguments": {
    "code": "(defun my-app:handle-edge-case (x)\n  (if (null x) \"NIL\" x))",
    "filename": "src/edge-case.lisp"
  }
}
```

### Step 2: Verify Fix Works

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:handle-edge-case nil)",
    "package": "MY-APP"
  }
}
```

### Step 3: Persist to File

```json
{
  "tool": "lisp-edit-form",
  "arguments": {
    "operation": "replace",
    "file_path": "src/edge-case.lisp",
    "form_type": "defun",
    "form_name": "handle-edge-case",
    "content": "(defun my-app:handle-edge-case (x)\n  (if (null x) \"NIL\" x))\n"
  }
}
```

## Workflow 5: Concurrent Modification

Modify code while threads are running.

### Step 1: Identify Active Threads

```json
{
  "tool": "thread_list"
}
```

Note which threads are using the code to be modified.

### Step 2: Check Thread-Safe Functions

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(bt:all-threads)",
    "package": "CL-USER"
  }
}
```

### Step 3: Apply Change

```json
{
  "tool": "code_replace_function",
  "arguments": {
    "function": "my-app:process-queue",
    "new-code": "(defun my-app:process-queue ()...)"
  }
}
```

### Step 4: Monitor Thread Behavior

```json
{
  "tool": "thread_list"
}
```

Verify threads continue functioning.

## Safety Patterns

### Pattern: Versioned Functions

```lisp
;; Instead of:
(defun compute (x) (* x x))

;; Use:
(defvar *compute-version* 1)

(defun compute (x)
  (format t "Using version ~d~%" *compute-version*)
  (* x x))
```

### Pattern: Feature Flags

```lisp
(defparameter *use-new-algorithm* nil)

(defun compute (x)
  (if *use-new-algorithm*
      (new-algorithm x)
      (old-algorithm x)))
```

### Pattern: Guarded Transitions

```lisp
(defvar *processing-lock* (bt:make-lock))

(defun update-handler (new-handler)
  (bt:with-lock (*processing-lock*)
    (setq *current-handler* new-handler)))
```

## Common Issues

### Issue: Old Code Still Running

**Symptom**: Changes not reflected in behavior

**Cause**: Threads using old function continue running

**Solution**:
- Wait for threads to complete current operation
- Or restart specific threads
- Use atomic replacement for functions

### Issue: Package Not Updated

**Symptom**: Symbol not found after reload

**Cause**: Package not re-imported

**Solution**:
```json
{
  "tool": "code_reload_package",
  "arguments": {
    "package": "MY-APP"
  }
}
```

### Issue: FASL Mismatch

**Symptom**: "Invalid Fasl File" error

**Cause**: SBCL version changed, old FASLs incompatible

**Solution**:
```json
{
  "tool": "code_reload_system",
  "arguments": {
    "system": "my-app",
    "force": true
  }
}
```

## Best Practices

1. **Test changes in REPL first** before applying to running system
2. **Use atomic replacement** for single function changes
3. **Package reload** for multi-file changes
4. **System reload** for dependency changes
5. **Monitor threads** during concurrent modification
6. **Have rollback plan** (keep old definitions accessible)
7. **Document changes** for future reference

## Approval Workflow

The following operations require user approval:

- `:compile-file` - Compiling new code
- `:modify-running-code` - Loading FASLs or replacing functions
- `:terminate-thread` - Thread operations

Request approval before proceeding:

```json
{
  "tool": "request_approval",
  "arguments": {
    "operation": "modify-running-code",
    "details": {
      "function": "my-app:critical-function",
      "file": "src/critical.lisp"
    }
  }
}
```

## See Also

- @prompts/debugging-workflows.md - Debugging issues found
- @agents/hot-reload-specialist.md - Live modification expert
- docs/tools/hot-reload.md - Complete hot reload tool reference
