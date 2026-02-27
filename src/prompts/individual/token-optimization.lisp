;;;; token-optimization.lisp
;;;; Prompt: Token Optimization Workflow

(in-package :cl-tron-mcp/prompts)

(define-prompt
    "token-optimization"
  "Token Optimization Workflow"
  "How to efficiently use the modular structure to minimize token usage."
  nil
  (list
   (list :role "user"
         :content
         (list :type "text"
               :text
               "How can I minimize token usage when working with this MCP?

## Token Optimization Principles

This MCP is designed for efficient token usage through modularization.
Follow these principles to reduce context by 50-70%.

### Principle 1: Load Only What You Need

The documentation is split into focused modules. Read only what's relevant:

**For debugging:**
```
resources/read :uri \"docs/agents/workflows.md\"
```
Then read only the debugging section.

**For profiling:**
```
resources/read :uri \"docs/agents/workflows.md\"
```
Then read only the profiling section.

**For hot-reload:**
```
resources/read :uri \"docs/agents/workflows.md\"
```
Then read only the hot-reload section.

### Principle 2: Use Guided Workflows

Instead of reading all documentation, use prompts:

```
prompts/get :name \"debugging-workflow\"
prompts/get :name \"profiling-workflow\"
prompts/get :name \"hot-reload-workflow\"
```

Each prompt gives you exactly what you need for that task.

### Principle 3: Discover Tools On-Demand

Don't read the full tool catalog. Use tools/list when needed:

```
tools/list
```

Then filter for relevant tools:
- Debugging: debugger_*, inspect_*, repl_*
- Profiling: profile_*, trace_*, health_check, runtime_stats
- Hot-reload: code_compile_string, reload_system

### Principle 4: Read Modular Documentation

The agent guide is split into 7 focused files:

| File | When to Read |
|------|--------------|
| docs/agents/getting-started.md | First time setup |
| docs/agents/workflows.md | Learning patterns |
| docs/agents/tool-reference.md | Looking up specific tools |
| docs/agents/conventions.md | Writing code |
| docs/agents/transport-logging.md | Transport issues |
| docs/agents/troubleshooting.md | Debugging problems |
| docs/agents/token-optimization.md | This guide |

### Principle 5: Use Resources for Deep Dives

When you need detailed information, use resources:

```
resources/list
resources/read :uri \"docs/agents/token-optimization.md\"
```

This loads documentation on-demand instead of including it in context.

## Example: Efficient Debugging Session

**Inefficient (loads everything):**
1. Read AGENTS.md (832 lines)
2. Read all tool documentation
3. Read all workflows

**Efficient (loads only what's needed):**
1. prompts/get :name \"debugging-workflow\"
2. tools/list (filter for debugger tools)
3. Use the tools directly

**Token savings:** 60-80%

## Summary

| Technique | Savings | How |
|-----------|---------|-----|
| Modular docs | 60-80% | Read only relevant sections |
| Guided workflows | 60-80% | Use prompts instead of full docs |
| On-demand tools | 50-70% | tools/list when needed |
| Lazy loading | 50-70% | resources/read on-demand |

## Best Practices

1. **Start with prompts** - Use prompts/get for your task
2. **Read selectively** - Only read relevant documentation sections
3. **Discover on-demand** - Use tools/list when you need tools
4. **Deep dive with resources** - Use resources/read for details

The modular structure is designed to minimize token usage while providing full functionality when needed."))))
