# Approval

Tools that can change the running Lisp image have **approval level `user`** (see
[tools.md](tools.md)). The server does not run them until a human approves.

## The exchange

1. `tools/call` with the tool name and arguments. The server replies with a result whose text is JSON:
   `{"approval_required": true, "request_id": "...", "message": "..."}`.
2. The **client UI** shows the request to the user, who approves or denies. The client sends
   `approval/respond` with `request_id`, `approved` (boolean) and an optional `message`.
3. If approved, re-send the **identical** `tools/call` (same name, same arguments) adding
   `approval_request_id` = the `request_id` and `approved: true`. The server runs the tool.
4. If denied, calling the tool again creates a new request.

## Rules the server enforces

- **Bound to the call.** An approval is valid only for the exact tool and arguments it was requested
  for. Re-sending with different arguments fails with the error "Approval expired or already used"
  and burns the approval.
- **Single use.** A second use of the same `request_id` fails.
- **Expires** after the approval timeout (300 s). Approving after expiry fails.
- **Whitelist.** If `whitelist_enable` is on and a pattern matches the tool, the tool runs without
  prompting. Patterns are case-insensitive *substring* matches on the tool name (so `repl` matches every
  `repl_*` tool). Keep patterns specific; `whitelist_status` shows the state.
- **Audit.** Operations are written to the audit log with sensitive parameters redacted.

## Trust boundary (read this)

`approval/respond` is an ordinary JSON-RPC method on the same channel as `tools/call`. The server cannot
tell a human click from an agent calling it. The guarantee therefore holds **only if your MCP client
withholds `approval/respond` from the model and routes it through a user prompt.** Agents must never call
`approval/respond` themselves, and operators should not rely on approval as a security boundary against
an untrusted model. Tron also listens only on loopback and rejects non-loopback `Host`/`Origin` headers
on the HTTP transport; it has no authentication, so do not expose its port.

## Not covered by approval

Read-only tools (inspection, backtraces, xref, health) run immediately, as does `repl_connect`.
`tools/list`, `resources/*` and `prompts/*` are always available.
