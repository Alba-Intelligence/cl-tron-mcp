# Performance Profiling Workflows

This guide covers performance profiling and analysis workflows for SBCL applications.

## Quick Reference

**Profiling Tool Cheat Sheet:**
| Task | Tool | Overhead | Use Case |
|------|------|----------|----------|
| Start profiler | `profile_start` | Medium (1-10x) | Function-level timing |
| Start statistical | `sprof_start` | Low (1-5%) | Hotspot identification |
| Stop profiler | `profile_stop` | N/A | End profiling session |
| Get report | `profile_report` | N/A | View results |
| Generate flamegraph | `profile_flamegraph` | Low | Visual analysis |
| Reset counters | `profile_reset` | N/A | Clear profiling data |

## Core Concepts

### Profiling Types

1. **Deterministic Profiling** (`sb-profile`)
   - Records every function call
   - High accuracy, higher overhead
   - Good for understanding call graphs

2. **Statistical Profiling** (`sb-sprof`)
   - Samples call stack periodically
   - Low overhead, statistical accuracy
   - Good for production use

3. **Flamegraph Visualization**
   - Visual representation of time spent
   - Great for identifying hotspots
   - Exportable for external analysis

## Workflow 1: Deterministic Profiling

### Step 1: Select Functions to Profile

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(profile 'my-app:compute\n             'my-app:process\n             'my-app:render)",
    "package": "MY-APP"
  }
}
```

Or profile all functions in package:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(profile-all)",
    "package": "MY-APP"
  }
}
```

### Step 2: Run Workload

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:run-workload)",
    "package": "MY-APP"
  }
}
```

### Step 3: Stop Profiling

```json
{
  "tool": "profile_stop"
}
```

### Step 4: Get Report

```json
{
  "tool": "profile_report",
  "arguments": {
    "format": "flat"
  }
}
```

Returns flat listing sorted by time.

```json
{
  "tool": "profile_report",
  "arguments": {
    "format": "callers"
  }
}
```

Returns functions sorted by who calls them.

### Step 5: Analyze Results

Key metrics:
- **Calls**: Number of function invocations
- **%time**: Percentage of total time
- **sec/call**: Seconds per call
- **cumul**: Cumulative seconds

## Workflow 2: Statistical Profiling

Lower overhead for production-like scenarios.

### Step 1: Start Statistical Profiler

```json
{
  "tool": "sprof_start",
  "arguments": {
    "max-samples": 10000,
    "interval": 0.001
  }
}
```

- `max-samples`: Maximum stack samples to collect
- `interval`: Seconds between samples (0.001 = 1ms)

### Step 2: Run Workload

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:run-production-workload)",
    "package": "MY-APP"
  }
}
```

### Step 3: Stop and Report

```json
{
  "tool": "sprof_report",
  "arguments": {
    "format": "flat"
  }
}
```

Returns percentage of samples in each function.

```json
{
  "tool": "sprof_report",
  "arguments": {
    "format": "call-graph"
  }
}
```

Returns call graph with edge weights.

## Workflow 3: Flamegraph Generation

Visual performance analysis.

### Step 1: Profile with Sampling

```json
{
  "tool": "sprof_start",
  "arguments": {
    "max-samples": 5000
  }
}
```

### Step 2: Run Workload

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:generate-report)",
    "package": "MY-APP"
  }
}
```

### Step 3: Generate Flamegraph

```json
{
  "tool": "profile_flamegraph",
  "arguments": {
    "output-path": "/tmp/flamegraph.svg",
    "sample-count": 5000
  }
}
```

### Step 4: View Flamegraph

Open the SVG file in a browser. Flamegraph shows:
- Width = time spent
- Colors = categories or samples
- Click to zoom for details

## Workflow 4: Memory Profiling

Identify memory allocation issues.

### Step 1: Enable Allocation Tracking

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(sb-profile:profile 'alloc)",
    "package": "CL-USER"
  }
}
```

### Step 2: Run Workload

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(my-app:process-data large-dataset)",
    "package": "MY-APP"
  }
}
```

### Step 3: Check Allocation Report

```json
{
  "tool": "profile_report",
  "arguments": {
    "format": "allocation"
  }
}
```

Shows bytes allocated per function.

## Workflow 5: Comparative Profiling

Compare performance before/after changes.

### Before Changes

```json
{
  "tool": "profile_reset"
}
```

Profile baseline:
```json
{
  "tool": "profile_start",
  "arguments": {
    "functions": ["my-app:compute"]
  }
}
```

Run workload and capture results:
```json
{
  "tool": "profile_report",
  "arguments": {
    "format": "flat"
  }
}
```

Save or note the results.

### After Changes

```json
{
  "tool": "profile_reset"
}
```

Profile again with same workload. Compare results.

## Interpreting Results

### Hotspots (High %time)

Functions taking most time. Focus optimization here.

### Call Frequency (High calls)

Functions called frequently. Consider caching or reducing calls.

### Deep Call Stacks

Many levels deep may indicate design issues or unnecessary abstraction.

### Unexpected Functions

Functions appearing in profile that shouldn't be expensive indicate bugs.

## Optimization Patterns

### Pattern 1: Reduce Function Call Overhead

**Before**:
```lisp
(defun inner-loop ()
  (dotimes (i 1000)
    (compute-small-thing i)))
```

**After** (inline computation):
```lisp
(defun inner-loop ()
  (dotimes (i 1000)
    ;; Inlined computation
    (* i i)))
```

### Pattern 2: Cache Expensive Computations

**Before**:
```lisp
(defun expensive (x)
  (compute-heavy x))

(defun process (list)
  (mapcar #'expensive list))
```

**After** (memoization):
```lisp
(defvar *cache* (make-hash-table))

(defun expensive (x)
  (or (gethash x *cache*)
      (setf (gethash x *cache*)
            (compute-heavy x))))
```

### Pattern 3: Avoid Unnecessary Consing

**Before** (creates list each time):
```lisp
(defun add-result (x)
  (push x *results*))
```

**After** (use arrays or vectors):
```lisp
(defvar *result-count* 0)
(defvar *results* (make-array 1000 :fill-pointer 0))

(defun add-result (x)
  (vector-push-extend x *results*))
```

## Common Issues

### Issue: Profile Shows Only Top-Level

**Cause**: Inner functions not being profiled

**Solution**: Profile entire package:
```json
{
  "tool": "profile_start",
  "arguments": {
    "functions": ["my-app"]
  }
}
```

### Issue: Overhead Too High

**Cause**: Profiling adds significant overhead

**Solution**: Use statistical profiler:
```json
{
  "tool": "sprof_start"
}
```

### Issue: Can't Find Problem

**Cause**: Issue may be in system libraries

**Solution**: Profile with library support:
```json
{
  "tool": "profile_start",
  "arguments": {
    "include-libraries": true
  }
}
```

### Issue: Results Inconsistent

**Cause**: JIT or caching affecting results

**Solution**: Warm up first:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(dotimes (_ 10) (my-app:run-workload))",
    "package": "MY-APP"
  }
}
```

## Best Practices

1. **Profile before optimizing** - Don't guess, measure
2. **Use representative workload** - Realistic data and usage patterns
3. **Warm up first** - JIT compilation affects results
4. **Profile multiple runs** - Average out variance
5. **Compare before/after** - Quantify improvement
6. **Use appropriate profiler** - Statistical for production, deterministic for development
7. **Document baseline** - Track performance over time

## See Also

- @prompts/production-monitoring.md - Ongoing performance monitoring
- @agents/performance-engineer.md - Performance optimization expert
- docs/tools/profiler.md - Complete profiler tool reference
