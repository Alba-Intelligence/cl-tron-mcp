# Profiler Tools

Tools for performance profiling and analysis of SBCL applications.

## Tools Overview

| Tool | Purpose | Overhead | Approval Required |
|------|---------|----------|------------------|
| `profile_start` | Start deterministic profiling | Medium | No |
| `profile_stop` | Stop profiling | N/A | No |
| `profile_report` | Get profiling report | Low | No |
| `profile_reset` | Reset profile counters | No | No |
| `sprof_start` | Start statistical profiling | Low | No |
| `sprof_report` | Get statistical profile | Low | No |
| `profile_flamegraph` | Generate flamegraph | Low | No |

## Profiling Types

### Deterministic Profiling (sb-profile)
- Records every function call
- High accuracy
- Medium overhead (1-10x)
- Use for development debugging

### Statistical Profiling (sb-sprof)
- Samples call stack periodically
- Statistical accuracy
- Low overhead (1-5%)
- Use for production-like scenarios

## profile_start

### Overview
Start deterministic profiling for specific functions.

### Tool Definition
```json
{
  "name": "profile_start",
  "description": "Start deterministic profiling",
  "parameters": {
    "type": "object",
    "properties": {
      "functions": {
        "type": "array",
        "description": "Functions to profile",
        "items": {
          "type": "string"
        }
      },
      "package": {
        "type": "string",
        "description": "Profile all functions in package"
      }
    }
  }
}
```

### Usage Examples

**Profile specific functions:**
```json
{
  "tool": "profile_start",
  "arguments": {
    "functions": ["my-app:compute", "my-app:process"]
  }
}
```

**Profile all functions in package:**
```json
{
  "tool": "profile_start",
  "arguments": {
    "package": "MY-APP"
  }
}
```

## sprof_start

### Overview
Start low-overhead statistical profiling.

### Tool Definition
```json
{
  "name": "sprof_start",
  "description": "Start statistical profiling",
  "parameters": {
    "type": "object",
    "properties": {
      "max_samples": {
        "type": "integer",
        "description": "Maximum samples to collect",
        "default": 10000
      },
      "interval": {
        "type": "number",
        "description": "Seconds between samples",
        "default": 0.001
      }
    }
  }
}
```

## profile_report

### Overview
Generate a profiling report.

### Tool Definition
```json
{
  "name": "profile_report",
  "description": "Generate profiling report",
  "parameters": {
    "type": "object",
    "properties": {
      "format": {
        "type": "string",
        "enum": ["flat", "callers", "callees", "graph"],
        "default": "flat"
      }
    }
  }
}
```

### Report Formats

**Flat format:**
```
Function          Calls  %time  Sec/Call
--------------------------------------------
MY-APP:COMPUTE    10000   45.2   0.000045
MY-APP:PROCESS    5000    20.1   0.000040
```

**Callers format:**
```
Function          Caller                Calls  %time
------------------------------------------------------
MY-APP:COMPUTE   MY-APP:INNER          8000   36.2
MY-APP:COMPUTE   MY-APP:OUTER          2000    9.0
```

## profile_flamegraph

### Overview
Generate a flamegraph SVG for visual profiling.

### Tool Definition
```json
{
  "name": "profile_flamegraph",
  "description": "Generate flamegraph SVG",
  "parameters": {
    "type": "object",
    "properties": {
      "output_path": {
        "type": "string",
        "description": "Path for output SVG file"
      },
      "sample_count": {
        "type": "integer",
        "description": "Number of samples",
        "default": 5000
      }
    },
    "required": ["output_path"]
  }
}
```

### Usage Example
```json
{
  "tool": "profile_flamegraph",
  "arguments": {
    "output_path": "/tmp/flamegraph.svg",
    "sample_count": 10000
  }
}
```

## Workflow

```
┌─────────────────────────────────────────────────────────────┐
│                   Profiling Workflow                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. START        2. RUN WORKLOAD    3. STOP               │
│     profile_start   (normal usage)     profile_stop        │
│     sprof_start                                            │
│                                                             │
│                         ↓                                   │
│                                                             │
│  4. ANALYZE     5. OPTIMIZE       6. RE-PROFILE           │
│     profile_report   changes          profile_reset        │
│     flamegraph       apply            repeat               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Error Handling

### Common Errors

**No profiling data:**
```json
{
  "error": {
    "code": -32000,
    "message": "No profiling data available",
    "data": {
      "type": "NO_DATA"
    }
  }
}
```

**Invalid format:**
```json
{
  "error": {
    "code": -32000,
    "message": "Invalid report format",
    "data": {
      "type": "INVALID_FORMAT"
    }
  }
}
```

## See Also

- @prompts/profiling-analysis.md - Complete profiling workflows
- @agents/performance-engineer.md - Performance optimization expert
- docs/examples/performance-patterns.md - Optimization examples
