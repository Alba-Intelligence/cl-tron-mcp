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

## Principles

1. **Start with a prompt, not the docs.** prompts/get with the workflow you need
   (debugging-workflow, hot-reload-workflow, profiling-workflow, or a persona prompt).
2. **Read one short document.** resources/read :uri \"docs/using/quickstart.md\" is enough to start.
3. **Discover tools on demand.** tools/list returns the exact schemas; docs/using/tools.md is a
   one-line-per-tool catalogue grouped by category. Filter by prefix:
   - Debugging: repl_*, inspect_*, breakpoint_*
   - Profiling: profile_*, trace_*, health_check, runtime_stats
   - Hot-reload: repl_compile, code_compile_string, reload_system
4. **Deep dives only when needed.** Per-tool details: resources/read :uri \"docs/tools/<tool-name-with-dashes>.md\".
   Error codes: docs/using/errors.md. Troubleshooting: docs/using/troubleshooting.md.
5. **Do not re-read.** The long-running Lisp session keeps state; query it (repl_eval, inspect_*)
   instead of reloading files or documentation.

## Documents

| Document | When to read |
|----------|--------------|
| docs/using/quickstart.md | First, always |
| docs/using/tools.md | Choosing a tool |
| docs/using/approval.md | A tool returned approval_required |
| docs/using/errors.md | A tool returned an error code |
| docs/using/troubleshooting.md | Connection or startup problems |
| docs/using/clients.md | Configuring your MCP client |"))))
