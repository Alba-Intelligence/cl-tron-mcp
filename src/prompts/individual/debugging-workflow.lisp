;;;; debugging-workflow.lisp
;;;; Prompt: Debugging Workflow

(in-package :cl-tron-mcp/prompts)

(define-prompt
    "debugging-workflow"
  "Debugging Workflow"
  "Step-by-step process for debugging errors in running Lisp code."
  nil
  (list
   (list :role "user"
         :content
         (list :type "text"
               :text
               "I encountered an error in my Common Lisp code. How do I debug it?

## The Debugging Workflow

When an error occurs in SBCL, the debugger is automatically invoked.
Here's how to use Tron MCP to debug it:

### Step 1: Trigger the Error

Evaluate the problematic code:

```
swank_eval :code \"(my-buggy-function 42)\"
```

The response will indicate an error occurred, and the debugger is active.

### Step 2: Get the Backtrace

See where the error occurred:

```
swank_backtrace
```

This shows the call stack, helping you identify the problematic function.

### Step 3: Get Available Restarts

See your recovery options:

```
swank_get_restarts
```

Common restarts include:
- **ABORT** - Return to top level
- **RETRY** - Try the operation again
- **USE-VALUE** - Provide a different value
- **CONTINUE** - Continue past the error

### Step 4: Fix the Code

You have two options:

**Option A: Invoke a Restart**
```
swank_invoke_restart :restart_index 1
```

**Option B: Hot-Fix the Code**
```
swank_eval :code \"(defun my-buggy-function (x)
                     (if (< x 0) 0 (* x 2)))\"
```

### Step 5: Verify the Fix

Run the code again:

```
swank_eval :code \"(my-buggy-function 42)\"
```

### Step 6: Persist the Fix

Remember to also fix the source file! The hot-patch only exists in memory.

## Pro Tips

- **Don't panic** - The debugger is your friend
- **Session persists** - State is not lost on errors
- **Use frame locals** - `swank_frame_locals` shows variable values
- **Inspect objects** - `swank_inspect` lets you explore data"))))
