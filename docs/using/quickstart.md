# Quickstart for agents using Tron

Tron is an MCP server that controls a **long-running SBCL image** over Swank: evaluate, debug,
inspect, profile and hot-reload live code. All program state lives in that image, not in Tron.
**Never restart the image to apply a change; patch it live.**

```
Your SBCL + Swank  <--Swank-->  Tron (MCP server)  <--MCP-->  you (the agent)
   port 4005                      HTTP port 4006 or stdio
```

## 1. Connect

The target image must run Swank. Either the user starts it:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
```

or you launch one: `swank_launch` (needs approval; `port` defaults to the configured Swank port,
4005). Then call `repl_connect` (`host` default `127.0.0.1`,
`port` default `4005`) and verify with `repl_eval` of `(+ 1 2)`. `repl_status` shows the connection.

## 2. The loop

`explore -> experiment -> persist -> verify -> hot-reload`

| I want to... | Use |
|---|---|
| Run code | `repl_eval` (`code`, `package`) |
| Load a definition | `repl_compile` (`code`, `package`, `filename`) or `code_compile_string` |
| Reload an ASDF system | `reload_system` |
| Understand a symbol | `repl_describe`, `repl_doc`, `repl_completions`, `inspect_function`, `inspect_class` |
| Who calls / references X | `who_calls`, `who_references`, `who_binds`, `who_sets`, `list_callees` |
| See an error | `repl_backtrace`, `repl_frame_locals` (`frame`), `repl_get_restarts` |
| Recover | `repl_invoke_restart` (`restart_index`), `repl_continue`, `repl_abort` |
| Step | `repl_step`, `repl_next`, `repl_out` |
| Break / trace | `repl_set_breakpoint`, `breakpoint_*`, `trace_function`, `trace_list` |
| Inspect an object | `repl_inspect`, `inspect_object` |
| Profile | `profile_start`, `profile_stop`, `profile_report` |
| Health | `health_check`, `runtime_stats`, `thread_list`, `gc_run` |

Full list with approval levels: [tools.md](tools.md). Parameters: call `tools/list`. Failures carry a
`code`; see [errors.md](errors.md) and [troubleshooting.md](troubleshooting.md).

**Results.** A tool result is a JSON object in the `text` content, with lowercase `snake_case` keys
(`in_debugger`, `dropped_events`). Flags that are false are `false`; empty lists are `[]`. When a tool
reports an error the response also has `isError: true`. Unknown extra arguments are ignored. A tool that
runs longer than the tool timeout (30 s) fails with JSON-RPC error `-32008`; the work may already have
had side effects, so inspect the image before retrying. `swank_debugger_state` includes
`dropped_events` (how many Swank events were discarded because the queue overflowed).

## 3. Approval

Tools that change the running image (eval, compile, reload, breakpoints, tracing, profiling, launch,
kill...) return `approval_required` instead of running. A human must approve; read
[approval.md](approval.md) for the exact exchange. Approving needs a confirmation code the user reads from the Tron server terminal; ask the user for it, never look for it yourself.

## 4. Debugging an error (typical)

1. `repl_eval` returns a debugger event, or `repl_status` shows you are in the debugger.
2. `repl_backtrace`, then `repl_frame_locals` for the failing frame, then `repl_get_restarts`.
3. Fix the definition with `repl_compile`; keep the state.
4. Recover with `repl_invoke_restart` or `repl_continue`; re-run the failing call to verify.
5. Persist the fix in the source file; the live patch alone is lost on restart.

## 5. Conventions and pitfalls

- JSON keys are lowercase. Parameter names in `tools/list` are authoritative (some are snake_case,
  some are lowercased camelCase such as `systemname`).
- Package-qualify symbols or pass `package`. Package locks protect `CL` and SBCL internals.
- Keep calls short-running: the server handles one request at a time, so a long evaluation
  delays every other request.
- More guided workflows: `prompts/list`, then `prompts/get` (`getting-started`, `debugging-workflow`,
  `hot-reload-workflow`, `profiling-workflow`, `sbcl-debugging-expert`, `hot-reload-specialist`,
  `performance-engineer`). Other docs: `resources/list`.
