# Tron Workflow Examples

Concrete examples for common tasks. Each example uses **unified `repl_*` tools** with the Swank backend. See [tutorial/e2e-mcp-workflow.md](../tutorial/e2e-mcp-workflow.md) for a minimal "first connection + first eval + first error + first restart" narrative.

## Example 1: Debug a Runtime Error

**Scenario:** Code throws an error. Find and fix the bug without restarting.

### Step 1: Check Connection
```
Tool: repl_status
Expected: connected; or connect first with repl_connect
```

### Step 2: Run the Failing Code
```
Tool: repl_eval
Arguments: { "code": "(my-buggy-function 7)" }
Result: Error with debugger info
```

### Step 3: Get the Backtrace
```
Tool: repl_backtrace
Result: Stack frames showing error location
```

### Step 4: Examine the Error
Look at frame 0 in the backtrace to see:
- Which function failed
- The condition that was raised
- Available restarts

### Step 5: Fix the Bug
```
Tool: repl_eval
Arguments: { "code": "(defun my-buggy-function (n) (corrected code here))" }
```

### Step 6: Abort and Retry
```
Tool: repl_invoke_restart
Arguments: { "restart_index": 2 }  ; Usually ABORT

Tool: repl_eval
Arguments: { "code": "(my-buggy-function 7)" }
Result: Correct output
```

**What we learned:** The factorial example had `(1)` instead of `1` as the else branch, causing NIL to be passed to `*`. Fixed by correcting the expression.

---

## Example 2: Profile Slow Code

**Scenario:** A function is slow. Find where time is spent.

### Step 1: Start Profiling
```
Tool: profile_start
```

### Step 2: Run the Slow Code
```
Tool: repl_eval
Arguments: { "code": "(process-large-dataset *data*)" }
```

### Step 3: Stop Profiling
```
Tool: profile_stop
```

### Step 4: Get the Report
```
Tool: profile_report
Arguments: { "format": "flat" }
```

### Step 5: Analyze
Look for:
- Functions with high self-time (hot spots)
- Functions called many times (opportunities for caching)
- Surprising call patterns

### Step 6: Optimize and Verify
Fix the hotspot, then re-profile to confirm improvement.

---

## Example 3: Find Who Calls a Function

**Scenario:** Need to understand dependencies before modifying a function.

### Step 1: Find Callers
```
Tool: who_calls
Arguments: { "symbol_name": "my-package:process-item" }
```

### Step 2: Review Results
The result shows all functions that call `process-item`.

### Step 3: Find Callees (What It Calls)
```
Tool: list_callees
Arguments: { "symbol_name": "my-package:process-item" }
```

### Step 4: Make Informed Changes
Now you know:
- What will break if you change the signature (callers)
- What you need to understand (callees)

---

## Example 4: Hot-Reload a Fix

**Scenario:** Found a bug, fix it in the running image.

### Step 1: Define the Fix
```
Tool: swank_eval
Arguments: { "code": "(defun my-function (x) (corrected-body x))" }
```

### Step 2: Verify the Fix
```
Tool: swank_eval
Arguments: { "code": "(my-function test-input)" }
```

### Step 3: Persist the Fix
Edit the source file to match. The running image now has the fix.

---

## Example 5: Inspect an Object

**Scenario:** Need to understand the structure of a complex object.

### Step 1: Get the Object
```
Tool: swank_eval
Arguments: { "code": "*my-complex-object*" }
Result: Includes object ID
```

### Step 2: Inspect the Object
```
Tool: inspect_object
Arguments: { "object_id": "42", "max_depth": 3 }
```

### Step 3: Drill Down
For nested structures, use the returned slot IDs with `inspect_slot`.

---

## Example 6: Thread Debugging

**Scenario:** Multi-threaded code has a race condition.

### Step 1: List All Threads
```
Tool: swank_threads
```

### Step 2: Get Thread Backtrace
```
Tool: swank_backtrace
Arguments: { "thread_id": "worker-3" }
```

### Step 3: Analyze Thread State
Look for:
- Threads waiting on locks
- Threads in unexpected states
- Race conditions in shared state access

---

## Example 7: Trace Function Calls

**Scenario:** Need to see the execution flow.

### Step 1: Add Trace
```
Tool: trace_function
Arguments: { "function_name": "my-package:process" }
```

### Step 2: Run Code
```
Tool: swank_eval
Arguments: { "code": "(process something)" }
```

### Step 3: Check Trace Output
Trace output appears in the Swank output stream.

### Step 4: Remove Trace
```
Tool: trace_remove
Arguments: { "function_name": "my-package:process" }
```

---

## Quick Reference: Error Recovery

| Situation | Tool | Action |
|-----------|------|--------|
| Code threw error | `swank_backtrace` | See where it failed |
| Want to retry | `swank_invoke_restart` | Use RETRY restart |
| Want to abort | `swank_invoke_restart` | Use ABORT restart |
| Want to provide value | `swank_invoke_restart` | Use USE-VALUE restart |
| Need to fix first | `swank_eval` | Define corrected function, then abort/retry |

## Quick Reference: Argument Names

All tools use underscore_case for keyword arguments:

| JSON key | Lisp keyword |
|----------|--------------|
| `symbol_name` | `:SYMBOL-NAME` |
| `class_name` | `:CLASS-NAME` |
| `object_id` | `:OBJECT-ID` |
| `restart_index` | `:RESTART-INDEX` |
| `function_name` | `:FUNCTION-NAME` |

## Quick Reference: Result Access

Results are property lists. Access with `getf`:

```lisp
(getf result :result)    ; The main result
(getf result :error)     ; Error flag (T if error)
(getf result :message)   ; Error message or status
(getf result :value)     ; Evaluated value
(getf result :frames)    ; Debugger frames
(getf result :restarts)  ; Available restarts
```
