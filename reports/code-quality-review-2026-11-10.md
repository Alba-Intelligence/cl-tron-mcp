# Code Quality Review — 2026-11-10

**Method:** static read of `AGENTS.md`, `agents/`, `docs/`, `reports/`, `prompts/`, and the
security / protocol / transport / swank / hot-reload / sbcl / tools / resources sources.
**Not done:** nothing was executed (`sbcl` is blocked in the review sandbox), so the test
suite was not run. Every finding below cites code I read; items marked *(unverified at runtime)*
are inferences. Run `(asdf:test-system :cl-tron-mcp)` before acting on them.

Scope note: `src/tools/validation.lisp` has uncommitted WIP in the main checkout (duplicated
`validate-*-param` helpers); it is reviewed only as a duplication issue (D2).

---

## 0. Status update (same day, after rebasing onto `origin/main` 39d00b4)

The review above was written against local `main` (baf9f89). `origin/main` is 8 commits ahead
(including a jonathan→jzon migration). Re-checked against that base:

| Finding | Status |
|---|---|
| S1 unauthenticated HTTP eval; Origin/Host guard | **Fixed** (feac90f) |
| S2 approval not bound to call; S3 expiry not enforced | **Fixed** (feac90f), tests pass |
| B6 any tool error disconnects Swank | **Fixed upstream** (cdf0f60; log now says "connection preserved") |
| S4, S5, S6, B1, B2, B3, B4, B5, B7, B8, P*, D* | Still open (B1, B2, B5, B7 re-confirmed in source) |

Test suite (SBCL 2.6.8, `scripts/run-tests.lisp`): all non-environmental tests pass, including the
new ones. Five tests fail only in this environment: four launch `start-mcp.sh`/ECL subprocesses
(one is killed with exit 137) and one needs `devenv`/`run-mcp.sh`. The suite also did not compile
before this change: tests referenced the removed `cl-tron-mcp/unified` package and unexported
symbols (fixed). Note `docs/agents/conventions.md` still says Jonathan; the code now uses jzon.

---

## 1. What the project is (one paragraph)

Tron is an MCP server (stdio / HTTP, JSON-RPC 2.0) that acts as a *Swank client* to a
long-lived SBCL image. ~11k lines of Lisp, ~90 tools in `repl_*` (preferred) and `swank_*`
layers, a server-enforced approval flow for mutating tools, MCP resources (whitelisted docs)
and prompts (guided workflows). Docs are extensive; the architecture (state lives in the
target image) is sound and clearly explained.

## 2. Findings, ranked

### Critical — security

**S1. Unauthenticated, approval-free `eval` over HTTP.**
`src/transport/http-hunchentoot.lisp` (`/lisply/lisp-eval`) → `src/transport/http.lisp:36`
`lisp-eval-handler` does `(eval (read-from-string code))` *inside the MCP server process*.
It bypasses the whole approval system, runs with `*read-eval*` on, and is live in the default
`combined` transport. Bound to 127.0.0.1, but any web page can still reach it: a browser
`fetch('http://127.0.0.1:4006/lisply/lisp-eval', {method:'POST', body:'{"code":"..."}'})`
with `Content-Type: text/plain` is a CORS "simple request" — no preflight, and the handler
ignores `Content-Type` and `Origin`. That is drive-by RCE from any site the user visits.
Also: the `output` string-stream is created but never bound to `*standard-output*`, so
"stdout" capture is dead code, and evaluated output goes to the real stdout — corrupting the
JSON-RPC stream in combined mode.
*Fix:* delete the endpoint, or route it through `call-tool` + approval. Independently: reject
requests whose `Origin`/`Host` isn't loopback/expected, require `Content-Type: application/json`,
and add an optional bearer token (`TRON_HTTP_TOKEN`) for `/rpc`/`/mcp`.

**S2. Approval is not bound to the operation approved.**
`handle-approval-reinvocation` (`src/tools/handlers-tools.lisp`) consumes an approved
`request_id` and then runs whatever `tool-name`/`arguments` the *re-invocation* carries.
Approve a harmless `repl_eval "(+ 1 2)"`, then replay the ID with `(uiop:run-program "rm -rf ...")`
or another tool entirely. The request record already stores `:tool` and `:arguments`
(`request-approval`), they just are not compared.
*Fix:* store a hash of (tool, canonical arguments) at request time; on consume, require equality
and also check `expires` (see S3).

**S3. Approval expiry is never enforced; cleanup is never scheduled.**
`approval-response` ignores `approval-request-expires`; `cleanup-all-approvals` is defined and
exported but has no caller (grep). So the documented 300 s timeout does not exist: a request
can be approved hours later, and `*pending-approvals*` / `*approved-request-ids*` grow
unbounded.
*Fix:* check `expires` in `approval-response` and `consume-approved-request-id`; call cleanup
opportunistically from `request-approval`.

**S4. The agent can approve itself.**
`approval/respond` is an ordinary JSON-RPC method on the same channel as `tools/call`. The
`approval_required` response hands the `request_id` to the agent, which can immediately call
`approval/respond {approved:true}`. This is inherent to the design as documented; the
"human in the loop" holds only if the MCP *client* withholds that method from the model.
*Fix options:* (a) use MCP elicitation / a client-side confirmation instead; (b) have the
server issue a separate out-of-band approval secret (e.g. printed to stderr or served at
a human-only HTTP path) that `approval/respond` must present; (c) at minimum, document this
trust boundary prominently in `docs/` (currently AGENTS.md presents approval as a security
guarantee).

**S5. Whitelist matching is loose and unaudited.**
`whitelist-match-p` string patterns are case-insensitive *substring* matches on the tool name,
so pattern `"repl"` whitelists every `repl_*` tool (including `repl_eval`). Patterns can also
be arbitrary functions. Whitelist hits are not written to the audit log.
*Fix:* exact match on tool name by default (glob if needed); log every whitelist bypass.

**S6. `read` with `*read-eval*` enabled on remote-controlled strings.**
`replace-function` (`read-from-string function-name`), `compile-and-load` (`read` loop),
`safe-eval` (see B3), `lisp-eval-handler`. Any `#.(...)` in agent-supplied text executes at read
time, *before* eval and before any approval-level reasoning about "what the code does".
Bind `*read-eval*` to `nil` around every `read` of external text.

### High — correctness bugs

**B1. `replace-function` can never replace anything.** `src/hot-reload/core.lisp`:
```lisp
(unwind-protect (progn (eval new-code) (list :success t ...))
  (setf (symbol-function symbol) old-def))   ; cleanup form = runs on SUCCESS too
```
The cleanup form executes on normal exit, so the new definition is always rolled back and the
tool reports `:success t`. Rollback belongs in a `handler-case`/error path only. (The
`code_replace_function` workflow documented in `agents/hot-reload-specialist.md` therefore
silently does nothing on the local fallback path.)

**B2. Several hot-reload tools are stubs that report success.** `get-package-source-files`
returns `nil` ("Placeholder implementation"), so `reload-package` reports
`:success t :files-reloaded 0`. `get-source-location` returns only the symbol name. Either
implement via `asdf`/`sb-introspect:find-definition-source` or return an explicit
"not implemented" error — a false success is worse for an agent than an error.

**B3. `safe-eval` timeout/binding is broken.** `src/sbcl/eval.lisp`:
`(setf *read-eval* nil)` is executed *after* `read-from-string` (too late, and a global
`setf` that leaks); the eval runs in a new thread so the `let`-bound `*package*` is not
inherited (threads see global values); polling uses `get-universal-time` (1 s resolution)
with `sleep 0.05`; and `bt:destroy-thread` is used on timeout (unsafe; can corrupt locks).
Use `sb-ext:with-timeout`/`sb-sys:with-deadline` or `bt:interrupt-thread` + a catch tag, and
rebind specials inside the thread. Also `read-from-string` only evaluates the first form.
It currently has no callers beyond `eval-with-output`, so consider deleting it.

**B4. Tool timeout is not a timeout.** `execute-tool-with-timeout` measures elapsed time
*before* and *after* `call-tool`; it cannot interrupt a hung tool and the "before" check is
always false. A tool that blocks 10 minutes holds the (single) stdio reader for 10 minutes and
only then returns a timeout error. Run tools in a worker with a bounded wait
(`bt:join-thread` with timeout / `sb-thread:join-thread :timeout`) or document that tools
must self-limit.

**B5. `wait-for-response` timeout never fires if no notification arrives.**
`src/swank/swank-rpc.lisp`: the elapsed check sits in the loop body, but
`condition-wait` is called without `:timeout`, so if Swank hangs silently the thread blocks
forever — the exact failure mode `reports/code-review.md` (Feb 2026, items 1.3/2.1) said was
to be fixed. Use `(condition-wait cv lock :timeout remaining)`.

**B6. Any tool error disconnects Swank.** `cleanup-on-error` (called from both
`execute-tool-with-timeout` and `handle-tool-call`) calls `swank-disconnect` and
`clrhash *pending-requests*`. So a trivial validation error, a typo'd argument, or
an unknown keyword argument in *any* tool tears down the debugging session the whole design
exists to keep alive — and also drops other in-flight requests. Moreover `swank-disconnect`
does not reset `*repl-connected*` in `:cl-tron-mcp/tools` (`unified-tools.lisp`), so `repl_status`
then claims "connected" *(unverified at runtime)*. Scope cleanup to the failing request:
remove only that request's id; disconnect only on transport-level errors (EOF/socket errors).

**B7. Tool results are `~a`-printed Lisp plists, not JSON.**
`execute-tool-with-timeout` returns `(format nil "~a" result)` as the MCP text content. The
agent receives `(:SUCCESS T :PORT 4006 ...)` with uppercase keys, `NIL` for false, etc. —
contradicting `docs/agents/conventions.md` (“MCP responses MUST use lowercase JSON keys”).
Serialize with `jonathan:to-json` (plist → object) and set `isError` on error results,
per the MCP tools spec. *(Tool results only; the envelope is correct.)*

**B8. Handler lambdas reject unknown keys.** `define-validated-tool` generates
`(lambda (&key ,@params) ...)` without `&allow-other-keys`. Clients that send an extra or
differently-cased argument cause a Lisp `error`, which (via B6) also kills the Swank
connection. Also, the schema declares camelCase names (`:systemName`) while validation uses
snake_case (`system_name`); `normalize-argument-key` bridges this, but schema parameters that
are not repeated in `:validation` are interned from the *camelCase* keyword and can never be
bound *(unverified at runtime)*. Pick one canonical name per parameter.

### Medium — robustness / protocol conformance

- **P1. HTTP taskmaster is `single-threaded-taskmaster`** with
  `:persistent-connections-p t` — one idle keep-alive client blocks every other request
  (including `/health`). `*max-concurrent-connections*` is defined but unused. Use
  `one-thread-per-connection-taskmaster` with `:max-thread-count`.
- **P2. Rate limiting keys on `remote-addr`** — always `127.0.0.1` here, so it throttles the
  single legitimate client (60/min is easy to exceed with an agent) while giving no
  per-origin protection. `*request-counts*` is never pruned.
- **P3. stdio transport swallows errors silently.** On handler error the loop logs and
  *sends nothing*; a client waiting on that `id` hangs. Parse failures never yield `-32700`
  because the transport parses before `handle-message`. Handle one message at a time is also
  the root of B4's blast radius.
- **P4. HTTP error mapping.** Parse failure returns `"{}"` with 400 instead of a JSON-RPC
  error; notifications return `"{}"` with 200 instead of 202 with no body; internal errors
  return 200 with `id: nil`, losing correlation.
- **P5. Resource read allocates `make-string (file-length)`** (bytes) and fills with
  `read-sequence` characters — for multibyte UTF-8 the tail is NUL padding.
  `resource` descriptors in `list-resources` report `size 0`/`text/markdown` placeholders.
  Use `uiop:read-file-string`.
- **P6. Event queue overflow** semantics (`*max-event-queue-size*`: "dropping oldest") vs
  `make-array :adjustable t :fill-pointer 0` — dropping from the front of a vector is O(n)
  and silently loses `:debug` events, which are exactly the ones an agent cannot afford to miss.
  Prefer a bounded ring/queue and surface a "dropped N events" marker in the next response.
- **P7. Managed-process child pipes are never drained.** `launch-sbcl-with-swank` uses
  `:output :stream :error-output :stream` and nothing reads them; a chatty child will fill
  the pipe buffer (64 KB) and block. Redirect to a log file or `/dev/null`.

### Maintainability / duplication

- **D1. `get-unix-time` is defined six times** (security/approval, security/audit, swank/swank-connection,
  monitor/core, profiler/core, protocol/handlers-utils), with at least two different
  implementations (`local-time` vs `sb-ext:get-time-of-day`). Put one in `cl-tron-mcp/core`
  utils (or a tiny shared package) and import it.
- **D2. `validate-*-param` duplicated** between `:cl-tron-mcp/protocol` and
  `:cl-tron-mcp/tools` (and the uncommitted WIP copies more). The comment says this avoids a
  circular dependency — but `handlers-tools.lisp` lives in the `tools` package and calls
  protocol-level helpers (`make-error-response`, `cleanup-on-error`, `timeout-error`, ...).
  The real fix is to move `handlers-tools.lisp` into `src/protocol/` (where the AGENTS.md
  project tree and `handlers.lisp` header say it is) and let `protocol` depend on `tools`
  one-way.
- **D3. Two tool-definition systems.** `define-tool` (macro with a *placeholder handler*,
  alexandria hash-tables, keyword-cased JSON schema) and `define-validated-tool`/`define-simple-tool`.
  If `define-tool` is unused, delete it; its handler body is literally
  `(list :result ,name :called t)`.
- **D4. `src/tools/package.lisp` contains functions, not a package** and several are
  dead or wrong: `get-tool-names-by-category` calls `(symbol-name name)` on string keys,
  `normalize-tool-description` is unreadable and mangles text, `validate-tool-availability`
  interns tool names as keywords while the registry is keyed by strings (always nil),
  `get-available-tools` has a malformed docstring/`mapcar #'car` over non-conses. Delete or fix.
- **D5. `tool-entry` / metadata helpers** (`update-tool-metadata`, `get-tool-metadata`) reference
  `tool-entryp` and `tool-entry-concurrency` which the struct does not define.
- **D6. `*pending-requests*` in the protocol package** is written on every call and read
  nowhere except `cleanup-on-error` (which `clrhash`es it); it is `eql`-keyed while JSON-RPC ids
  may be strings. Remove it or implement real cancellation with it.
- **D7. Eval-time side effects.** `swank-connection.lisp` runs `(ql:quickload :usocket)` inside
  `eval-when` — the `.asd` should declare `:usocket`/`:bordeaux-threads` instead; quickloading
  during compile breaks offline/CI builds and ASDF's dependency graph.
- **D8. Repository hygiene.** `reports/*.log` and `.oh-my-opencode-pi-stats.json` should be in
  `.gitignore`; `tests/test-*.lisp` helper scripts (no `deftest`) live beside real suites;
  `docs/examples/` is empty.

### Tests & docs

- **T1. Weak assertions.** e.g. `security-test.lisp` mostly checks `operation-requires-approval`
  membership and audit-log length. There is no test for: approval binding/expiry (S2/S3),
  `replace-function` actually replacing (B1), `wait-for-response` timing out (B5), tool error not
  disconnecting (B6), `/lisply/lisp-eval` being unreachable without auth (S1). Each finding above
  is a ready-made regression test; write them first (red), then fix.
- **T2. CI exists but its signal is unverified.** `.github/workflows/ci.yml` runs Rove on SBCL
  2.4/2.5/2.6 via `40ants/setup-lisp`. Check that it is green on `main`, that `rove:run` failures
  fail the job (a bare `rove:run` inside `--eval` may not set a non-zero exit status — wrap it
  with `(uiop:quit (if (rove:run ...) 0 1))`), and that the repo's Nix/devenv setup and CI
  use the same SBCL.
- **K1. Docs drift.** Counts disagree (“91 tools”, “14 categories”, per-file counts in AGENTS.md
  tree vs docs); `docs/mcp-resources-prompts.md` documents `file://` URIs but the response
  `size`/`mimeType` are placeholders; `reports/code-review.md` (Feb) lists blockers whose status
  is not tracked — add a "status" column (fixed/open/wontfix, commit) to it, then link from
  `reports/README.md`. Generate the tool count from the registry in a test and assert it
  against AGENTS.md instead of hand-editing.
- **K2. Docs token overhead.** `CLAUDE.md` → `AGENTS.md` → nine `@`-included files (thousands of lines)
  loads on every session; the “token optimization” work reduced tool *schemas* but the
  agent-instruction layer is large. Keep AGENTS.md ≤150 lines and let `resources/read` serve the
  rest on demand.

## 3. Proposed order of work

| # | Change | Why first | Effort |
|---|--------|-----------|--------|
| 1 | Remove/gate `/lisply/lisp-eval`; Origin+Host check; optional token (S1) | Remote-triggerable RCE | S |
| 2 | Bind approval to tool+args, enforce expiry, schedule cleanup (S2, S3) | Makes the approval model real | S |
| 3 | Fix `replace-function`; stub tools return explicit errors (B1, B2) | Tools silently lie | S |
| 4 | Scope `cleanup-on-error`; `&allow-other-keys` (B6, B8) | Preserves the session invariant | S |
| 5 | `condition-wait :timeout` (B5); real tool timeout (B4) | Long-running-session reliability | M |
| 6 | JSON tool results + `isError` (B7) | MCP conformance, agent parsing | M |
| 7 | `*read-eval*` nil everywhere (S6, B3) | Defence in depth | S |
| 8 | Taskmaster, stdio error replies, HTTP status codes (P1–P4) | Robustness | M |
| 9 | De-duplicate `get-unix-time`, validators, move `handlers-tools.lisp` (D1–D6) | Cleans the dependency knot | M |
| 10 | Regression tests per finding; verify CI exit status (T1, T2) | Locks it in | M |

Document S4 (agent can self-approve) regardless of what is decided — users should know the
actual trust boundary.

## 4. What is already good

- Clear architecture doc; state stays in the target image; honest "Caveats" section.
- Approval flow is server-side, one-time-use IDs, with locks around the shared tables.
- Resource server uses a whitelist plus truename-prefix check (path traversal considered).
- Separate packages per concern, consistent naming, docstrings on most public functions,
  Swank client is split into connection / RPC / events / API layers.
- Per-tool and error-code docs (`docs/tools/`, `docs/errors/`) with lazy hints is a good pattern.
