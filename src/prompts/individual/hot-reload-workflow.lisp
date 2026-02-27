;;;; hot-reload-workflow.lisp
;;;; Prompt: Hot Reload Workflow

(in-package :cl-tron-mcp/prompts)

(define-prompt
    "hot-reload-workflow"
  "Hot Reload Workflow"
  "How to modify running code without restarting the Lisp session."
  nil
  (list
   (list :role "user"
         :content
         (list :type "text"
               :text
               "I need to fix a bug in running code without restarting. How?

## Hot Reload Workflow

Tron MCP allows you to modify code in a running Lisp session,
just like Slime/Sly. This is essential for debugging production
issues or rapid development iteration.

### Step 1: Identify the Bug

Find the problematic function:

```
swank_eval :code \"(my-function 10)\"
```

If there's an error, use `swank_backtrace` to locate it.

### Step 2: Fix the Source File

Edit your source file with the fix:

```lisp
;; In my-app.lisp
(defun my-function (x)
  \"Fixed version with proper error handling.\"
  (check-type x number)
  (* x 2))
```

### Step 3: Reload the Function

**Option A: Evaluate the Definition**
```
swank_eval :code \"(defun my-function (x)
                     (check-type x number)
                     (* x 2))\"
```

**Option B: Compile the String**
```
swank_compile :code \"(defun my-function (x)
                       (check-type x number)
                       (* x 2))\"
              :filename \"my-app.lisp\"
```

**Option C: Reload the System**
```
reload_system :system_name \"my-app\"
```

### Step 4: Verify the Fix

Test the fixed function:

```
swank_eval :code \"(my-function 10)\"
```

Expected: 20

### Step 5: Commit Your Changes

Remember to commit the source file changes!

## Important Notes

- **Memory vs. File** - Hot patches are in-memory only
- **Persist changes** - Always update the source file
- **Redefined functions** - Old references may still point to old code
- **Use reload_system** - For comprehensive updates

## Workflow Tips

1. **Small changes** - Fix one thing at a time
2. **Test immediately** - Verify each change
3. **Update source** - Keep files in sync with image
4. **Use version control** - Easy to track what changed"))))
