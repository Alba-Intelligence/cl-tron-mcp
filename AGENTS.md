# AGENTS.md - working on Tron

Tron (`cl-tron-mcp`) is a Model Context Protocol server written in Common Lisp. It connects to a
long-running SBCL image over Swank and lets an AI agent evaluate, debug, inspect, profile and hot-reload
live code. This file is for **people and agents changing the Tron codebase**.

> **Using Tron as an agent (not changing it)?** Read [docs/using/quickstart.md](docs/using/quickstart.md),
> or ask the running server: `prompts/get discover-mcp`, then `resources/read` that file.

## Orientation

```
Your SBCL + Swank (port 4005)  <-- Swank -->  Tron  <-- MCP: stdio, or HTTP on port 4006 -->  agent
```

All program state lives in the target image; Tron holds connection state only. Never design a feature that
needs the image restarted.

| Need | Read |
|---|---|
| Install, start, transports | [docs/starting-the-mcp.md](docs/starting-the-mcp.md) |
| Architecture and source map | [docs/architecture.md](docs/architecture.md), [docs/code-reference.md](docs/code-reference.md), [docs/DEVELOPERS.md](docs/DEVELOPERS.md) |
| **How we work** (branch, test, docs, reports) | [docs/developing/workflow.md](docs/developing/workflow.md) |
| Coding conventions | [docs/developing/conventions.md](docs/developing/conventions.md) |
| Add or change a tool | [docs/developing/adding-a-tool.md](docs/developing/adding-a-tool.md) |
| Transports, HTTP security, logging | [docs/developing/transports.md](docs/developing/transports.md) |
| Tool list (generated) | [docs/using/tools.md](docs/using/tools.md) |
| Plans and roadmap | [docs/plans/](docs/plans/) |
| Reviews, findings, status | [reports/README.md](reports/README.md) |

## Commands

```bash
# Tests (exit code is the verdict). Needs OpenSSL on LD_LIBRARY_PATH outside `devenv shell`.
sbcl --noinform --non-interactive --load scripts/run-tests.lisp

# Regenerate generated docs after changing tools or error codes
sbcl --noinform --non-interactive --load scripts/gen-docs.lisp

# Run the server
./start-mcp.sh                  # combined: stdio + HTTP 4006
./start-mcp.sh --stdio-only     # what stdio MCP clients launch
./start-mcp.sh --http-only
```

REPL equivalents: `(ql:quickload :cl-tron-mcp)`, `(cl-tron-mcp/core:start-server :transport :stdio-only)`.
ECL/devenv subprocess tests need those tools on `PATH`; report them separately. Known: ECL fails to compile
`src/installation/installation.lisp` (review finding E1).

## Source map

```
src/core, config      server lifecycle, configuration, token tracking
src/transport         stdio, HTTP (Hunchentoot, loopback only), websocket (placeholder)
src/protocol          JSON-RPC dispatch
src/tools             registry, macros, all tool definitions, tools/call + approval handlers
src/swank             Swank client: connection, RPC, events, API, managed processes
src/security          approval flow, whitelist, audit log
src/resources         MCP resources (doc whitelist), error codes
src/prompts           MCP prompts (the canonical agent workflows)
src/debugger inspector hot-reload profiler tracer monitor xref logging sbcl   tool back-ends
tests/                Rove suites (docs-test.lisp checks docs against code)
scripts/              run-tests, gen-docs, launch helpers
docs/using/           for agents using Tron   docs/developing/  for contributors
```

## Rules

1. **Security first.** Anything that evaluates code, changes the image or starts processes must have
   `:requires-approval t`. Approvals are bound to the exact call, single-use and expire (see
   [docs/using/approval.md](docs/using/approval.md)). Never add an endpoint that evaluates code without
   approval. Bind `*read-eval*` to `nil` when reading agent-supplied text. HTTP binds to loopback and
   rejects non-loopback `Host`/`Origin`; do not weaken that.
2. **Stdio purity.** In stdio mode only JSON-RPC may reach stdout. Log to stderr (log4cl).
3. **Tests.** Fix bugs test-first. A task is done only when its tests ran and passed; otherwise say
   UNVERIFIED. Do not start the next task on unverified work.
4. **Docs are code.** No hand-typed tool counts or lists; tool names in docs must exist; links must
   resolve; generated files must be fresh (all enforced by `tests/docs-test.lisp`). Planned tools go in
   `docs/plans/roadmap.md`.
5. **One source of truth.** Agent workflows live in `src/prompts/individual/*.lisp`; do not copy them into
   markdown.
6. **Reports.** Findings go in `reports/YYYY-MM-DD-<slug>.md` with a status table and IDs; reference IDs in
   commits. Do not publish a report of an unfixed vulnerability before the fix is merged.
7. **Git.** One task per branch/worktree off `origin/main`; small commits; never push to `main`, never
   force-push; end commits with the project's `Co-Authored-By` trailer.
8. **Style.** Google CL style plus [docs/developing/conventions.md](docs/developing/conventions.md): 2-space
   indent, docstrings on public functions, lowercase JSON keys written as `:|key|`.
