# Workflows

## Recommended Workflow: One Long-Running Lisp Session

For the MCP to interact with Swank the same way a user in Slime would—see output, debugger state, step, move frames, invoke restarts, inspect, compile—use a **single long-running Lisp session** that the user (or automation) starts and keeps running.

### Two Processes

1. **Lisp session (Swank)**
   The user starts one SBCL (or other Lisp) with Swank and leaves it running. All code loading and execution (by the user or by the MCP) happens in this process. The debugger runs here; Slime/Sly/Emacs can attach to the same session.

2. **MCP server**
   Started by the MCP client (Cursor, Kilocode, Opencode) via `start-mcp.sh` or equivalent. It runs in a separate process and connects to the Lisp session as a **Swank client**. The MCP then uses Swank facilities (eval, backtrace, restarts, stepping, inspect, etc.) over the protocol—the same way Slime does.

### Agent Workflow

1. User starts the Lisp session with Swank (e.g. `(swank:create-server :port 4006)`).
2. User (or client) starts the MCP server; the agent connects to the Lisp session via `repl_connect` or `swank_connect` (or the client config is set so the MCP connects on startup).
3. The agent uses `repl_eval`, `repl_backtrace`, `repl_inspect`, and related tools to load code, run it, see output and debugger state, step, move frames, invoke restarts, and fix code—all through the connected session. No second REPL; one session, MCP as a client of it.

See **docs/architecture.md** and **README.md** (Swank Integration / Recommended setup) for step-by-step setup and tool usage.

### Unified vs Swank

- After connecting, use **unified `repl_*` tools only** (do not mix with `swank_*`) to reduce mental load.
- **Dedicated port for MCP:** Use a separate Swank port for MCP (e.g. Swank on 4006) so the user keeps 4005 for their editor. Example: `(swank:create-server :port 4006 :dont-close t)` for MCP; keep 4005 for Slime.

## Common Workflows

| Task               | Tools                                                             | Pattern                              |
| ------------------ | ----------------------------------------------------------------- | ------------------------------------ |
| **Evaluate code**  | `swank_eval`                                                      | Send code string, get result         |
| **Debug error**    | `swank_eval` → error → `swank_backtrace` → `swank_invoke_restart` | Trigger error, see frames, fix       |
| **Inspect object** | `inspect_object`, `inspect_class`                                 | Get object ID, inspect slots         |
| **Profile code**   | `profile_start` → run code → `profile_stop` → `profile_report`    | Measure, analyze                     |
| **Hot-fix bug**    | `swank_compile` or `swank_eval` with `defun`                      | Redefine function in running session |
| **Find callers**   | `who_calls`                                                       | Cross-reference analysis             |

## When You See an Error

1. Don't panic - the debugger is active
2. Use `swank_backtrace` to see stack frames
3. Use `swank_get_restarts` to see options
4. Either fix the code and `swank_eval` again, or `swank_invoke_restart` to abort
5. The session persists - state is not lost

## Restarts

- Use **`repl_get_restarts`** and **`repl_invoke_restart`** (unified) instead of `swank_get_restarts` / `swank_invoke_restart` in normal workflows.

## Core Development Loop

```
EXPLORE → EXPERIMENT → PERSIST → VERIFY → HOT-RELOAD
          ↑                              ↓
          └────────── REFINE ───────────┘
```

## Typical Development Session

When the MCP is connected to a long-running Lisp session (Swank):

1. **Explore**: Use tools to understand current state (`repl_eval`, `repl_backtrace`, `repl_inspect`, etc.).
2. **Experiment**: Test in the connected REPL with `repl_eval`.
3. **Persist**: Edit files and compile with `code_compile_string` or `repl_compile`.
4. **Verify**: Run tests in the session (e.g. `repl_eval` with `(asdf:test-system :cl-tron-mcp)`).
5. **Debug**: Use Swank facilities via MCP tools—backtrace, restarts, step, frame up/down, inspect—the same way a user would in Slime.

Without a connected REPL, tools that require it (e.g. `repl_eval`) return "Not connected to any REPL"; use `repl_connect` or `swank_connect` first. See Recommended Workflow above and docs/architecture.md.

## Tool Usage Order

```
SEARCH → READ → UNDERSTAND → EDIT → COMPILE → VERIFY
   ↓        ↓          ↓         ↓         ↓         ↓
clgrep   lisp-read   inspect   code_      compile   tests
                    object    compile    string
```