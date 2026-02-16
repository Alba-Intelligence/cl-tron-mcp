;;;; src/prompts/handler.lisp
;;;;
;;;; Implementation of MCP Prompts for cl-tron-mcp.
;;;;
;;;; This file provides:
;;;;   - Prompt listing (prompts/list)
;;;;   - Prompt retrieval (prompts/get)
;;;;   - Predefined workflows for Common Lisp development
;;;;
;;;; Prompts are guided workflows that help AI agents use the MCP server
;;;; correctly. They address the discoverability problem where agents
;;;; didn't know how to connect to Swank or use the debugging tools.
;;;;
;;;; Usage:
;;;;   MCP clients call prompts/list to see available workflows
;;;;   MCP clients call prompts/get with a prompt name to get instructions

(in-package :cl-tron-mcp/prompts)

;;; ============================================================
;;; Condition Types
;;; ============================================================

(define-condition prompt-not-found-error (error)
  ((name :initarg :name :reader prompt-not-found-name))
  (:report (lambda (condition stream)
             (format stream "Prompt not found: ~a"
                     (prompt-not-found-name condition)))))

;;; ============================================================
;;; Prompt Registry
;;; ============================================================

(defvar *prompt-registry* (make-hash-table :test 'equal)
  "Registry of available prompts, keyed by name (string).")

(defstruct prompt-descriptor
  "Structure representing an MCP prompt."
  name
  title
  description
  arguments
  messages)

(defun define-prompt (name title description arguments messages)
  "Register a prompt in the registry.
NAME is the prompt identifier (string).
TITLE is a human-readable title.
DESCRIPTION explains what the prompt does.
ARGUMENTS is a list of argument specifications (can be nil).
MESSAGES is a list of message content (the prompt text)."
  (setf (gethash name *prompt-registry*)
        (make-prompt-descriptor
         :name name
         :title title
         :description description
         :arguments arguments
         :messages messages)))

;;; ============================================================
;;; Predefined Prompts
;;; ============================================================

(defun initialize-default-prompts ()
  "Register all default prompts.
Called when the module is loaded."
  
  ;; Getting Started - Connection workflow
  (define-prompt
   "getting-started"
   "Getting Started with Tron MCP"
   "Step-by-step guide to connect to Swank and verify the MCP server is working correctly."
   nil
   (list
    (list :role "user"
          :content
          (list :type "text"
                :text
                "I want to use Tron MCP for Common Lisp development. How do I get started?

Follow these steps:

## Step 1: Check if Swank is Running

First, check if a Swank server is already running:

```bash
ss -tlnp | grep 4005
```

If you see output, Swank is running. Skip to Step 3.

## Step 2: Start Swank Server

If Swank is not running, start it in your SBCL session:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
```

This creates a persistent Swank server that Tron can connect to.

## Step 3: Connect to Swank

Use the swank_connect tool:

```
swank_connect :host \"127.0.0.1\" :port 4005
```

Or use the unified interface (auto-detects Swank/nrepl):

```
repl_connect :port 4005
```

## Step 4: Verify Connection

Test the connection with a simple expression:

```
swank_eval :code \"(+ 1 2)\"
```

Expected result: \"3\"

## Step 5: You're Ready!

You now have a persistent Lisp session. You can:
- Evaluate code with swank_eval
- Debug errors with swank_backtrace and swank_get_restarts
- Hot-reload code with swank_compile
- Profile performance with profile_start/stop

## Important Notes

- **Never restart the SBCL session** - All state lives there
- **Tron is a client** - It connects to your running SBCL
- **Session persists** - Variables, functions, and state remain across calls
- **Use resources/list** to see available documentation"))))

  ;; Debugging Workflow
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

  ;; Hot Reload Workflow
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

  ;; Profiling Workflow
  (define-prompt
   "profiling-workflow"
   "Profiling Workflow"
   "How to profile Common Lisp code to find performance bottlenecks."
   nil
   (list
    (list :role "user"
          :content
          (list :type "text"
                :text
                "My Common Lisp code is slow. How do I profile it?

## Profiling Workflow

Tron MCP provides deterministic profiling to identify performance
bottlenecks in your code.

### Step 1: Start Profiling

Begin the profiling session:

```
profile_start
```

This enables timing for all subsequent function calls.

### Step 2: Run Your Code

Execute the code you want to profile:

```
swank_eval :code \"(process-large-data my-dataset)\"
```

The profiler records all function calls and their timing.

### Step 3: Stop Profiling

End the profiling session:

```
profile_stop
```

### Step 4: Analyze Results

Get the profiling report:

```
profile_report :format \"flat\"
```

Available formats:
- **flat** - Simple list of functions by time
- **graph** - Call tree with timing
- **cumulative** - Total time per function

### Step 5: Identify Hotspots

Look for:
- Functions with high total time
- Functions called many times
- Functions with high average time

### Step 6: Optimize

Focus on the hottest functions:

1. **Algorithm** - Can you use a better algorithm?
2. **Data structures** - Are you using the right ones?
3. **Memory** - Is GC a factor?
4. **Type declarations** - Can you add type hints?

### Step 7: Re-measure

After optimization, profile again to verify improvement.

## Example Session

```
1. profile_start
2. swank_eval :code \"(benchmark-my-app)\"
3. profile_stop
4. profile_report :format \"graph\"
```

## Tips

- **Profile realistic data** - Use production-like data
- **Multiple runs** - Run several times for consistency
- **Isolate** - Profile specific functions, not everything
- **Before/after** - Always measure after changes"))))

  ;; Initialize on load
  )

(initialize-default-prompts)

;;; ============================================================
;;; Prompt Listing (prompts/list)
;;; ============================================================

(defun list-prompts ()
  "Return a list of all registered prompts.
Implements the MCP prompts/list operation."
  (loop for prompt being the hash-values of *prompt-registry*
        collect prompt))

(defun handle-prompts-list (id)
  "Handle MCP prompts/list request.
Returns plist suitable for JSON serialization."
  (let ((prompts (list-prompts)))
    (list :|jsonrpc| "2.0"
          :|id| id
          :|result|
          (list :|prompts|
                (loop for prompt in prompts
                      collect (list :|name| (prompt-descriptor-name prompt)
                                   :|title| (prompt-descriptor-title prompt)
                                   :|description| (prompt-descriptor-description prompt)
                                   :|arguments| (prompt-descriptor-arguments prompt)))))))

;;; ============================================================
;;; Prompt Retrieval (prompts/get)
;;; ============================================================

(defun get-prompt (name)
  "Get a prompt by name.
Returns the prompt descriptor or signals PROMPT-NOT-FOUND-ERROR."
  (let ((prompt (gethash name *prompt-registry*)))
    (unless prompt
      (error 'prompt-not-found-error :name name))
    prompt))

(defun handle-prompts-get (id params)
  "Handle MCP prompts/get request.
PARAMS should contain :|name| with the prompt name.
Returns plist suitable for JSON serialization."
  (let ((name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (declare (ignore arguments))
    (handler-case
        (let ((prompt (get-prompt name)))
          (list :|jsonrpc| "2.0"
                :|id| id
                :|result|
                (list :|description| (prompt-descriptor-description prompt)
                      :|messages| (prompt-descriptor-messages prompt))))
      (prompt-not-found-error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32602
                   :|message| (format nil "Prompt not found: ~a"
                                      (prompt-not-found-name e)))))
      (error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32603
                   :|message| (princ-to-string e)))))))

;;; ============================================================
;;; Help
;;; ============================================================

(defun prompts-help ()
  "Return help text for the prompts module."
  (list :module "cl-tron-mcp/prompts"
        :description "MCP Prompts implementation for guided workflows"
        :operations (list
                    (list :name "prompts/list"
                          :description "List all available guided workflows")
                    (list :name "prompts/get"
                          :description "Get a specific workflow by name"))
        :available-prompts (loop for name being the hash-keys of *prompt-registry*
                                 collect name)))

(provide :cl-tron-mcp/prompts)
