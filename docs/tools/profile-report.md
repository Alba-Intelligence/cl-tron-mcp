# profile_report

**Short Description:** Get profiling report

**Full Description:** Get profiling report in specified format. Formats: flat (simple list), graph (call tree), cumulative (total time per function). Use after profile_start/stop.

**Parameters:**
- `format`: Report format: "flat", "graph", or "cumulative" (optional)

**Returns:** Profiling report in specified format

**Example Usage:**
```lisp
(profile_report :format "flat")
(profile_report :format "graph")
```

**Notes:** Use after profile_start and profile_stop. Different formats provide different views of the profiling data.