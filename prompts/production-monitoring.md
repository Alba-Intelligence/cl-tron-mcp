# Production Monitoring Guide

This guide covers production monitoring and health check workflows for SBCL applications.

## Quick Reference

**Monitoring Tool Cheat Sheet:**
| Task | Tool | Interval | Purpose |
|------|------|----------|---------|
| Memory stats | `memory_stats` | On-demand | Memory usage |
| GC stats | `gc_stats` | On-demand | Garbage collection |
| Runtime stats | `runtime_stats` | On-demand | Overall statistics |
| Health check | `health_check` | Continuous | Service health |
| Metrics export | `metrics_export` | Periodic | Prometheus format |
| Thread list | `thread_list` | On-demand | Thread status |

## Core Concepts

### Health Dimensions

1. **Memory** - Heap usage, GC pressure, allocation rate
2. **Threads** - Active threads, blocked threads, thread states
3. **GC** - Collection frequency, pause times, reclamation rate
4. **Resources** - File descriptors, network connections
5. **Availability** - Service responding, error rates

## Workflow 1: Basic Health Check

### Step 1: Check Overall Health

```json
{
  "tool": "health_check"
}
```

Returns composite health status:
- `:healthy` - All systems operational
- `:degraded` - Some metrics outside thresholds
- `:critical` - Immediate attention required

### Step 2: Check Component Health

```json
{
  "tool": "memory_stats"
}
```

Returns:
- Total memory allocated
- Memory by generation
- Bytes freed by GC

```json
{
  "tool": "runtime_stats"
}
```

Returns:
- Uptime
- Thread count
- GC time percentage

## Workflow 2: Memory Monitoring

### Step 1: Get Current Memory State

```json
{
  "tool": "memory_stats"
}
```

Key metrics:
- **Total allocated**: Total bytes in heap
- **Generation 0**: Recent allocations
- **Generation 1**: Survived one collection
- **Generation 2**: Old objects

### Step 2: Monitor Over Time

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(defparameter *mem-samples* '())",
    "package": "CL-USER"
  }
}
```

Sample periodically:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(push (cons (get-internal-real-time)\n                     (sb-kernel:dynamic-usage))\n             *mem-samples*)",
    "package": "CL-USER"
  }
}
```

### Step 3: Analyze Trends

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(let ((samples (reverse *mem-samples*)))\n  (format t \"~%Memory samples:~%\")\n  (dolist (s (last samples 10))\n    (format t \"~6f seconds: ~d MB~%\"\n            (/ (car s) internal-time-units-per-second)\n            (/ (cdr s) 1048576))))",
    "package": "CL-USER"
  }
}
```

## Workflow 3: GC Monitoring

### Step 1: Check GC Statistics

```json
{
  "tool": "gc_stats"
}
```

Returns:
- Number of GC runs per generation
- Time spent in GC
- Bytes reclaimed
- Average pause time

### Step 2: Monitor GC Pressure

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(defparameter *gc-samples* '())",
    "package": "CL-USER"
  }
}
```

Sample after significant operations:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(push (sb-ext:gc-run-time) *gc-samples*)",
    "package": "CL-USER"
  }
}
```

### Step 3: Tune GC Parameters

```json
{
  "tool": "gc_tune",
  "arguments": {
    "generation-sizes": [67108864 268435456 1073741824],
    "promotion-thresholds": [0.6 0.95]
  }
}
```

## Workflow 4: Thread Monitoring

### Step 1: List All Threads

```json
{
  "tool": "thread_list"
}
```

Returns:
- Thread ID
- Thread name
- Thread state (:running, :waiting, :blocked)
- Priority

### Step 2: Check Thread Health

```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(format t \"~%Thread Summary:~%\")\n(format t \"Total: ~d~%\" (length (bt:list-all-threads)))\n(format t \"Running: ~d~%\"\n        (count :running (bt:list-all-threads)\n               :key #'bt:thread-state))\n(format t \"Waiting: ~d~%\"\n        (count :waiting (bt:list-all-threads)\n               :key #'bt:thread-state))",
    "package": "CL-USER"
  }
}
```

### Step 3: Inspect Specific Thread

```json
{
  "tool": "thread_inspect",
  "arguments": {
    "thread-id": "worker-1"
  }
}
```

## Workflow 5: Prometheus Metrics Export

### Step 1: Export Metrics

```json
{
  "tool": "metrics_export"
}
```

Returns metrics in Prometheus text format:

```
# HELP cltron_heap_bytes_allocated Total heap bytes allocated
# TYPE cltron_heap_bytes_allocated gauge
cltron_heap_bytes_allocated 2.147483648e9

# HELP cltron_gc_pause_seconds GC pause time in seconds
# TYPE cltron_gc_pause_seconds histogram
cltron_gc_pause_seconds_bucket{le="0.001"} 100
cltron_gc_pause_seconds_bucket{le="0.01"} 150
cltron_gc_pause_seconds_bucket{le="0.1"} 200
cltron_gc_pause_seconds_bucket{le="1.0"} 210
cltron_gc_pause_seconds_bucket{le="+Inf"} 210
```

### Step 2: Scrape with Prometheus

Configure Prometheus to scrape the metrics endpoint.

### Step 3: Set Up Alerts

Example Prometheus alerts:

```yaml
groups:
- name: cltron-alerts
  rules:
  - alert: HighMemoryUsage
    expr: cltron_heap_bytes_allocated / cltron_heap_max_bytes > 0.9
    for: 5m
    annotations:
      summary: "Memory usage above 90%"

  - alert: FrequentGC
    expr: rate(cltron_gc_runs_total[5m]) > 10
    annotations:
      summary: "More than 10 GC runs per second"
```

## Workflow 6: Incident Response

### Step 1: Identify Issue

```json
{
  "tool": "health_check"
}
```

Check overall status.

### Step 2: Gather Diagnostics

```json
{
  "tool": "thread_dump_all"
}
```

Get stack traces of all threads.

```json
{
  "tool": "memory_stats"
}
```

Check memory pressure.

### Step 3: Identify Root Cause

```json
{
  "tool": "runtime_stats"
}
```

Check for:
- High CPU usage
- Many blocked threads
- Excessive GC time

### Step 4: Take Action

Options:
- **Restart thread**: `thread_kill` + recreate
- **Force GC**: `memory_gc`
- **Reload code**: `code_reload_system`
- **Full restart**: `save-lisp-and-die`

## Best Practices

### Monitoring Cadence

| Metric | Frequency | Threshold Warning | Threshold Critical |
|--------|-----------|-------------------|-------------------|
| Memory | Every 1 min | >70% | >90% |
| GC time | Every 1 min | >5% | >20% |
| Thread count | Every 1 min | >100 | >500 |
| Error rate | Every 10 sec | >1% | >10% |

### Alerting Rules

1. **Page immediately**:
   - Service not responding
   - Memory >95%
   - Thread exhaustion

2. **Warn**:
   - Memory >70%
   - GC time >10%
   - Elevated error rate

3. **Informational**:
   - High but stable metrics
   - Scheduled maintenance

### Documentation

- Document baseline metrics
- Track metric trends over time
- Note incidents and resolutions
- Update thresholds based on experience

## Common Issues

### Issue: Memory Growing Without Bound

**Diagnosis**:
```json
{
  "tool": "memory_stats"
}
```

Check generations. If Gen 0 growing but Gen 2 stable, may be allocation leak.

**Solution**:
```json
{
  "tool": "repl_eval",
  "arguments": {
    "code": "(sb-ext:gc :full t)",
    "package": "CL-USER"
  }
}
```

Then monitor to see if memory stabilizes.

### Issue: GC Pauses Too Long

**Diagnosis**:
```json
{
  "tool": "gc_stats"
}
```

Check pause times.

**Solution**:
```json
{
  "tool": "gc_tune",
  "arguments": {
    "generation-sizes": [134217728 536870912 2147483648]
  }
}
```

Increase heap sizes for smoother GC.

### Issue: Threads Stuck

**Diagnosis**:
```json
{
  "tool": "thread_dump_all"
}
```

Examine stack traces.

**Solution**:
```json
{
  "tool": "thread_kill",
  "arguments": {
    "thread-id": "stuck-thread"
  }
}
```

Restart functionality.

## See Also

- @prompts/profiling-analysis.md - Performance analysis
- docs/tools/monitor.md - Complete monitoring tool reference
- docs/deployment.md - Production deployment guide
