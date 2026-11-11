# Approval

Tools that can change the running Lisp image have **approval level `user`** (see
[tools.md](tools.md)). The server does not run them until a human approves.

## The exchange

1. `tools/call` with the tool name and arguments. The server replies with a result whose text is JSON:
   `{"approval_required": true, "request_id": "...", "message": "..."}`.
2. Tron prints the request and an 8-character **confirmation code** on its own terminal (stderr and the
   controlling TTY). The code is **never** in any tool result or response. Ask the user to read it to you
   (or to enter it in your client's approval dialog).
3. Send `approval/respond` with `request_id`, `approved` (boolean), `confirmation_code` (required to
   approve; not needed to deny) and an optional `message`.
4. If approved, re-send the **identical** `tools/call` (same name, same arguments) adding
   `approval_request_id` = the `request_id` and `approved: true`. The server runs the tool.
5. If denied, calling the tool again creates a new request.

## Rules the server enforces

- **Bound to the call.** An approval is valid only for the exact tool and arguments it was requested
  for. Re-sending with different arguments fails with the error "Approval expired or already used"
  and burns the approval.
- **Out-of-band confirmation.** Approving needs the code from the server terminal. Three wrong codes
  discard the request. Comparison is case-insensitive and constant-time.
- **Single use.** A second use of the same `request_id` fails.
- **Expires** after the approval timeout (300 s). Approving after expiry fails.
- **Whitelist.** If `whitelist_enable` is on and a pattern matches the tool, the tool runs without
  prompting. Patterns are case-insensitive and match the **whole** tool name; `*` is a glob
  (`repl_*`). A bare `repl` matches only a tool named `repl`. Keep patterns specific;
  `whitelist_status` shows the state.
- **Audit.** Operations are written to the audit log with sensitive parameters redacted. Every whitelist
  bypass is logged as operation `whitelist-bypass`.

## Trust boundary (read this)

`approval/respond` is an ordinary JSON-RPC method on the same channel as `tools/call`, so the server
cannot tell who sends it. What it can check is *knowledge of the confirmation code*, which only exists on
the server's terminal. An agent that cannot read that terminal cannot approve its own request.

This is as strong as the separation between the agent and the server's terminal. It does **not** hold if
the agent can read the server's stderr or `/dev/tty` (for example it runs in the same terminal, or has a
shell tool and can read the server's log), or can run code in the Tron process by other means.

`TRON_APPROVAL_TRUST_CLIENT=1` turns the code requirement off and restores the old behaviour, in which the
guarantee holds only if the MCP client withholds `approval/respond` from the model. Use it only for a
client that does that. Tron listens only on loopback and rejects non-loopback `Host`/`Origin` headers on
the HTTP transport; it has no authentication, so do not expose its port.

## Not covered by approval

Read-only tools (inspection, backtraces, xref, health) run immediately, as does `repl_connect`.
`tools/list`, `resources/*` and `prompts/*` are always available.
