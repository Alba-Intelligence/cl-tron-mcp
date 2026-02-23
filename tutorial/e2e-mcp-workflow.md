# End-to-End MCP Workflow

This tutorial walks from "nothing running" to "first connection, first eval, first error, first restart" using **unified `repl_*` tools only**. Use this as a copy-paste reference for agents.

## Prerequisites

- Swank running in your Lisp session, e.g. `(swank:create-server :port 4005 :dont-close t)` or a dedicated port for MCP (e.g. 4006).
- MCP server started by your client (Cursor, OpenCode, Kilocode). See [docs/starting-the-mcp.md](../docs/starting-the-mcp.md) if the MCP won't start.

## Step 1: Connect

Use the unified interface (Swank on the given port):

```
Tool: repl_connect
Arguments: { "port": 4005 }
```

Or with explicit type and host:

```
Tool: repl_connect
Arguments: { "type": "swank", "host": "127.0.0.1", "port": 4005 }
```

## Step 2: First Eval

```
Tool: repl_eval
Arguments: { "code": "(+ 1 2)" }
```

Expected: result `"3"` (or similar). If the tool requires user approval, the client will show an approval prompt; after approval, re-invoke with `approval_request_id` and `approved: true` as documented in AGENTS.md.

## Step 3: Trigger an Error (e.g. buggy factorial)

```
Tool: repl_eval
Arguments: { "code": "(defun buggy-fact (n) (if (zerop n) 1 (* n (buggy-fact (1- n)))))" }
```

Then:

```
Tool: repl_eval
Arguments: { "code": "(buggy-fact -1)" }
```

This can enter the debugger (infinite recursion or error depending on implementation).

## Step 4: Get Backtrace

```
Tool: repl_backtrace
Arguments: {}
```

Use the result to see stack frames and the error location.

## Step 5: Get Restarts

```
Tool: repl_get_restarts
Arguments: {}
```

Note the index of the restart you want (e.g. ABORT).

## Step 6: Invoke Restart

```
Tool: repl_invoke_restart
Arguments: { "restart_index": 0 }
```

(Use the index from the previous step; 0 is often ABORT.)

## Step 7: Hot-Fix and Verify

Fix the code (e.g. add a base case) and compile:

```
Tool: repl_compile
Arguments: { "code": "(defun buggy-fact (n) (if (<= n 0) 1 (* n (buggy-fact (1- n)))))" }
```

Then eval again:

```
Tool: repl_eval
Arguments: { "code": "(buggy-fact 5)" }
```

Expected: `"120"` (or equivalent).

## Summary

- Use **only** `repl_*` tools after connecting: `repl_connect`, `repl_eval`, `repl_backtrace`, `repl_get_restarts`, `repl_invoke_restart`, `repl_compile`, etc.
- Use `repl_*` after connecting via `repl_connect` or `swank_connect`; do not mix with raw `swank_*` in normal workflows.
- See [prompts/workflow-examples.md](../prompts/workflow-examples.md) for more examples and [AGENTS.md](../AGENTS.md) for approval and tool reference.
