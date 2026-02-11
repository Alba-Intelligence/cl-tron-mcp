# Monitoring Tools

Tools for production monitoring and health checks.

## Tools Overview

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `memory_stats` | Memory usage | No |
| `gc_stats` | GC statistics | No |
| `runtime_stats` | Runtime statistics | No |
| `health_check` | Overall health | No |
| `metrics_export` | Prometheus metrics | No |
| `thread_dump_all` | All thread stacks | No |
| `gc_tune` | Tune GC parameters | Yes (:modify-running-code) |

## memory_stats

### Overview
Get memory usage statistics.

### Tool Definition
```json
{
  "name": "memory_stats",
  "description": "Get memory usage statistics"
}
```

### Return Value
```json
{
  "total_bytes": 2147483648,
  "by_generation": [
    {"generation": 0, "bytes": 67108864},
    {"generation": 1, "bytes": 268435456},
    {"generation": 2, "bytes": 1073741824}
  ],
  "bytes_freed": 536870912
}
```

## health_check

### Overview
Check overall system health.

### Tool Definition
```json
{
  "name": "health_check",
  "description": "Basic health check"
}
```

### Return Value
```json
{
  "status": "healthy",
  "checks": {
    "memory": "ok",
    "threads": "ok",
    "gc": "ok"
  },
  "uptime_seconds": 3600,
  "timestamp": "2026-02-11T10:00:00Z"
}
```

## metrics_export

### Overview
Export metrics in Prometheus format.

### Tool Definition
```json
{
  "name": "metrics_export",
  "description": "Export metrics for Prometheus"
}
```

### Return Value
```prometheus
# HELP cltron_heap_bytes_allocated Total heap bytes allocated
# TYPE cltron_heap_bytes_allocated gauge
cltron_heap_bytes_allocated 2.147483648e9

# HELP cltron_gc_runs_total Total GC runs
# TYPE cltron_gc_runs_total counter
cltron_gc_runs_total 42

# HELP cltron_threads_active Active threads
# TYPE cltron_threads_active gauge
cltron_threads_active 5
```

## See Also

- @prompts/production-monitoring.md - Complete monitoring workflows
