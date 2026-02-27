# profile_start

**Short Description:** Start profiling

**Full Description:** Start deterministic profiling. All function calls will be timed. Use to find performance bottlenecks. Remember to call profile_stop when done.

**Parameters:** None

**Returns:** Profiling start status

**Example Usage:**
```lisp
(profile_start)
```

**Notes:** Requires user approval. All function calls will be timed until profile_stop is called. Use for performance analysis.