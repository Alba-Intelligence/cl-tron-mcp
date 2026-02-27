# profile_stop

**Short Description:** Stop profiling

**Full Description:** Stop profiling and return report. Call after running the code you want to profile. Returns timing data for all functions called during profiling.

**Parameters:** None

**Returns:** Profiling report with timing data

**Example Usage:**
```lisp
(profile_stop)
```

**Notes:** Requires user approval. Call after running the code you want to profile. Returns detailed timing data for all functions.