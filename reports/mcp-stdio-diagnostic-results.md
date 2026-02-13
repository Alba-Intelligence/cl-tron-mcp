# MCP Stdio Diagnostic Results

Run date: 2026-02-13. These tests diagnose why the MCP server works from CLI but fails when launched by AI assistants (Kilocode, Opencode, Cursor).

## Test 1: Capture what client receives (stdio + start-mcp.sh)

**Command:** `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | bash -c "cd ... && ./start-mcp.sh" 2>/dev/null | head -30`

**Result:** First line of stdout is **not** JSON. Observed:
- "Starting CL-TRON-MCP..."
- "  SBCL: sbcl", "  Transport: stdio"
- "Available tools: 80", banner, "Starting the stdio MCP"
- Then SBCL banner and compilation messages

**Conclusion:** When using `start-mcp.sh`, the client receives many non-JSON lines before any Lisp output. **Root cause: script banner on stdout.**

---

## Test 2: Direct SBCL (no script)

**Command:** `echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | sbcl --non-interactive --eval '...' 2>/dev/null | head -20`

**Result:** First line of stdout is **not** JSON. Observed:
- SBCL banner (4 lines)
- "[MCP] Starting server with STDIO transport"
- "[MCP] Starting stdio transport (MCP protocol)"
- Then the JSON response

**Conclusion:** Even without the script, Lisp writes `[MCP]` messages and SBCL writes its banner to stdout. **Root cause: Lisp `format t` and SBCL startup banner on stdout.**

---

## Test 3: Notification pollution

**Command:** Sent `initialized` notification then `ping` request.

**Result:** Stdout contained the line: `Notification: initialized NIL`

**Conclusion:** The notification handler writes to stdout via `(format t "Notification: ...")`. **Root cause: notification handler pollutes JSON channel.**

---

## Test 4: Minimal environment (simulate IDE)

**Command:** `env -i HOME="$HOME" PATH="/usr/bin:/bin" bash -c '...'` — bash was not found (Nix system). With `PATH="$PATH"`, process started; first line still script banner.

**Conclusion:** On this Nix system, a truly minimal PATH (`/usr/bin:/bin`) does not include bash, so the script cannot run. With PATH preserved, startup works but stdout pollution remains.

---

## Test 5: Working directory from /tmp

**Command:** Ran `start-mcp.sh` from `/tmp` using absolute path to script.

**Result:** Server started. Script's `cd "$(dirname "$0")"` correctly changes to project dir.

**Conclusion:** Cwd is not the cause of failure when the script is invoked by absolute path.

---

## Test 6: Stderr vs stdout separation

**Command:** Redirected stdout and stderr to `tmp/mcp_stdout.txt` and `tmp/mcp_stderr.txt`.

**Result:**
- **stdout:** SBCL banner (4 lines), "[MCP] Starting server with STDIO transport", "[MCP] Starting stdio transport (MCP protocol)", then the JSON response line.
- **stderr:** Empty.

**Conclusion:** All non-JSON output (banner + [MCP] messages) goes to stdout. **Confirms stdout pollution; only JSON should be on stdout for stdio transport.**

---

## Summary of root causes confirmed

| Cause | Confirmed by |
|-------|----------------|
| Script banner on stdout when using `start-mcp.sh` | Test 1 |
| Lisp `format t` ([MCP] messages) on stdout | Tests 2, 6 |
| Notification handler writes to stdout | Test 3 |
| SBCL startup banner on stdout when stdin is pipe | Tests 2, 6 |
| Minimal PATH can prevent script from running (no bash) | Test 4 |
| Working directory not an issue when script path is absolute | Test 5 |

## Recommended fixes (for implementation later)

1. **Stdio transport:** Send all `[MCP]` and notification logs to `*error-output*` (stderr), not `format t` (stdout). Ensure no Lisp code writes to stdout except the single JSON line per response in `send-message-via-stdio`.
2. **start-mcp.sh:** When transport is stdio, do not echo to stdout (redirect script messages to stderr, or only echo when using HTTP/WebSocket).
3. **SBCL banner:** Consider starting SBCL with `--noinform` (or equivalent) so the startup banner goes to stderr or is suppressed when stdout is not a TTY (if supported).
