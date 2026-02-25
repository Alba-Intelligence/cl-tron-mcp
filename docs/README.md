# CL-TRON-MCP Documentation

## Getting Started

| Document                                            | Description                                                  |
| --------------------------------------------------- | ------------------------------------------------------------ |
| [Architecture](architecture.md)                     | How Tron works: one long-running Lisp session, MCP as client |
| [Swank Integration](swank-integration.md)           | Swank protocol implementation, wire format, RPC reference    |
| [MCP Resources & Prompts](mcp-resources-prompts.md) | Discoverability features for AI agents                       |

## Tool Documentation

| Document                                   | Tools Covered                                                                            |
| ------------------------------------------ | ---------------------------------------------------------------------------------------- |
| [tools/debugger.md](tools/debugger.md)     | `debugger_frames`, `debugger_restarts`, `breakpoint_*`, `step_frame`                     |
| [tools/inspector.md](tools/inspector.md)   | `inspect_object`, `inspect_slot`, `inspect_class`, `inspect_function`, `inspect_package` |
| [tools/hot-reload.md](tools/hot-reload.md) | `code_compile_string`, `reload_system`                                                   |
| [tools/profiler.md](tools/profiler.md)     | `profile_start`, `profile_stop`, `profile_report`                                        |
| [tools/threads.md](tools/threads.md)       | `thread_list`, `thread_inspect`, `thread_backtrace`                                      |
| [tools/monitor.md](tools/monitor.md)       | `health_check`, `runtime_stats`, `gc_run`, `system_info`                                 |

## Development Documentation

| Document                             | Description                                                         |
| ------------------------------------ | ------------------------------------------------------------------- |
| [DEVELOPERS.md](DEVELOPERS.md)       | Where to add features, map of the codebase, testing and conventions |
| [demo-creation.md](demo-creation.md) | How to create and regenerate demo GIFs with VHS                     |

## Workflow Guides

Located in `../prompts/`:

| Document                                                          | Description                            |
| ----------------------------------------------------------------- | -------------------------------------- |
| [workflow-examples.md](../prompts/workflow-examples.md)           | Step-by-step examples for common tasks |
| [debugging-workflows.md](../prompts/debugging-workflows.md)       | Debugging patterns and strategies      |
| [hot-reload-development.md](../prompts/hot-reload-development.md) | Hot-reload development workflow        |
| [profiling-analysis.md](../prompts/profiling-analysis.md)         | Performance profiling guide            |
| [production-monitoring.md](../prompts/production-monitoring.md)   | Production monitoring setup            |

## Quick Reference

### Start Swank Server

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

### Connect via MCP

```json
{ "name": "swank_connect", "arguments": { "port": 4006 } }
```

### Common Tools

```json
{"name": "swank_eval", "arguments": {"code": "(+ 1 2)"}}
{"name": "swank_backtrace"}
{"name": "swank_frame_locals", "arguments": {"frame": 0}}
{"name": "swank_invoke_restart", "arguments": {"restart_index": 2}}
```

### Run Tests

```lisp
(asdf:test-system :cl-tron-mcp)
```

## See Also

- **[AGENTS.md](../AGENTS.md)** - Quick start guide for AI agents
- **[README.md](../README.md)** - Project overview and installation
