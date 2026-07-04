# CL-TRON-MCP

`cl-tron-mcp` is a Model Context Protocol (MCP) server for Common Lisp development. It connects to a live Swank session and lets an MCP client inspect objects, read debugger state, evaluate code, hot-reload fixes, invoke restarts, and keep working inside the same Lisp image.

## What Tron Is For

Tron is built around the Common Lisp workflow that matters most for agentic programming:

1. keep one Lisp session running,
2. hit an error,
3. inspect the stack and locals,
4. compile a fix without restarting,
5. continue or restart from the debugger.

The richest workflow uses **SBCL + Swank**. Tron itself can run under SBCL or ECL, but the debugger-heavy path is designed around Swank-backed Common Lisp development.

## Architecture in One Picture

```text
SBCL + Swank  <---->  Tron (MCP server)  <---->  MCP client / AI agent
your code              tools/resources            Cursor, Copilot, etc.
debugger state         approval flow
loaded systems         stdio or HTTP transport
```

**Key rule:** the state lives in the Lisp session, not inside Tron. Tron is a client of that session.

## Quick Start

### 1. Install with Quicklisp

The easiest path is to clone the repo into Quicklisp local projects:

```bash
git clone https://github.com/Alba-Intelligence/cl-tron-mcp.git \
  ~/quicklisp/local-projects/cl-tron-mcp
cd ~/quicklisp/local-projects/cl-tron-mcp
```

If you keep the repo elsewhere, make sure Quicklisp can still see it by pushing the directory into `ql:*local-project-directories*` or by symlinking it into `~/quicklisp/local-projects/`.

### 2. Preload the System Once

This avoids the first-client-start timeout that can happen while SBCL compiles the project for the first time:

```bash
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(ql:quickload :cl-tron-mcp :silent t)'
```

### 3. Start a Swank Session

In the Lisp image you want the agent to work with:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

### 4. Start Tron

From the repository root:

```bash
./start-mcp.sh --stdio-only   # for stdio-based MCP clients
./start-mcp.sh                # long-running HTTP/combined mode
```

`start-mcp.sh` is the canonical runtime entrypoint. `run-mcp.sh` exists only as an optional convenience wrapper for `devenv` users.
For long-running modes, `Ctrl+C` now stops the launcher cleanly; `./start-mcp.sh --stop` also shuts down a running instance.

### 5. Verify the MCP Starts

```bash
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | \
  ./start-mcp.sh --stdio-only 2>/dev/null | head -1
```

You should receive a JSON-RPC response that includes `serverInfo` and the MCP protocol version.

### 6. Connect Tron to Swank

Once the MCP server is running, use either:

- `repl_connect` for the higher-level unified workflow, or
- `swank_connect` for lower-level Swank access.

Example tool call:

```json
{
  "name": "repl_connect",
  "arguments": { "port": 4006 }
}
```

## Tool Surface

The current registry exposes **91 tools** across **14 categories**, including:

- debugger tools (`debugger_*`, `breakpoint_*`, `step_frame`)
- unified REPL tools (`repl_*`)
- raw Swank tools (`swank_*`)
- hot-reload tools (`code_compile_string`, `reload_system`)
- inspection, profiling, tracing, monitoring, logging, xref, security, and managed Swank-process tools

See [docs/tools/index.md](docs/tools/index.md) for the full per-tool reference and [docs/code-reference.md](docs/code-reference.md) for the source-level map.

## Recommended Workflow

For interactive debugging and hot reload:

1. start the target Lisp image with Swank,
2. start Tron,
3. connect with `repl_connect`,
4. use `repl_eval`, `repl_backtrace`, `repl_frame_locals`, `repl_get_restarts`, and `repl_compile`,
5. continue with `repl_continue` or invoke a restart.

If you need to launch a disposable SBCL session from Tron itself, use the managed-process tools:

- `swank_launch`
- `swank_process_list`
- `swank_process_status`
- `swank_kill`

## Documentation Map

| Need | Read |
| --- | --- |
| Start/install/run Tron | [docs/starting-the-mcp.md](docs/starting-the-mcp.md) |
| Understand the architecture | [docs/architecture.md](docs/architecture.md) |
| Browse the full tool catalog | [docs/tools/index.md](docs/tools/index.md) |
| Learn the source layout | [docs/code-reference.md](docs/code-reference.md) |
| Contribute changes | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Work inside the codebase | [docs/DEVELOPERS.md](docs/DEVELOPERS.md) |
| Consume the MCP from an agent | [AGENTS.md](AGENTS.md) |

## Optional Nix / devenv Support

The project still ships a `devenv.nix` for contributors who want a reproducible shell with SBCL, ECL, and helper tools preinstalled. That is now an **optional development environment**, not the primary installation story.

## Current Caveats

- The best-supported debugger workflow is **SBCL + Swank**.
- The local hot-reload fallback is useful when no REPL is connected, but the full debugger/restart workflow requires a live Swank session.
- Some features are implementation-specific; the docs call those out where relevant.

## Testing

Run the existing suite with:

```lisp
(asdf:test-system :cl-tron-mcp)
```

Or from the shell:

```bash
cd ~/quicklisp/local-projects/cl-tron-mcp
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(pushnew (truename ".") ql:*local-project-directories* :test (function equal))' \
  --eval '(asdf:test-system :cl-tron-mcp)'
```
