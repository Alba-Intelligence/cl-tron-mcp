;;;; profiling-workflow.lisp
;;;; Prompt: Profiling Workflow

(in-package :cl-tron-mcp/prompts)

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
