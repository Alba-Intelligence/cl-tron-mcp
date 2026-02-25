# Starting the MCP Server

This document explains how the MCP server is started and how to fix common "MCP won't start" issues.

## Who Starts the MCP

- **The MCP client** (Cursor, OpenCode, Kilocode) starts the server when you add it to your config and (re)start the client. The client runs the configured command (e.g. `start-mcp.sh`).
- **The AI agent** only uses tools _after_ the server is running. The agent does not start the server; if the server fails to start, the cause is client config or environment.

## Portable Config Pattern

Copy the example that matches your client from **`examples/`** (e.g. `examples/cursor-mcp.json.example`). The examples use **tilde expansion** (`~`) for the standard Quicklisp path:

```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": "~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh",
      "args": ["--stdio-only"],
      "disabled": false
    }
  }
}
```

If your Quicklisp is in a different location, replace the path accordingly. Do not commit machine-specific paths; use the examples as templates.

### Why Tilde Expansion?

MCP clients (Cursor, VS Code, Kilocode) support tilde (`~`) expansion but **do not** support environment variables like `$HOME` in the command path. Using `~` allows configs to be portable across machines with the same Quicklisp setup.

## Server Detection (HTTP/Combined Modes)

For HTTP and combined modes, `start-mcp.sh` checks if a server is already running before starting a new one:

1. **PID file**: `.tron-server.pid` in the project directory tracks the running process
2. **Health check**: Probes `http://127.0.0.1:PORT/health` to verify the server is healthy
3. **Port check**: Detects if the port is in use by another process

If a healthy server is already running, the script exits successfully without starting a duplicate instance.

### Status and Stop Commands

```bash
./start-mcp.sh --status   # Check if server is running
./start-mcp.sh --stop     # Stop a running HTTP server
```

### Example Workflow for HTTP Mode

```bash
# Start the server (or use combined mode)
./start-mcp.sh --http-only

# Check if it's running
./start-mcp.sh --status
# Output: Server is RUNNING (PID: 12345, Port: 4006)

# Stop when done
./start-mcp.sh --stop
```

### Transport Modes (default: combined)

**Default:** `./start-mcp.sh` runs in **combined** mode: stdio (for MCP clients that start the server) and HTTP (on port 4006) at the same time. Use **`--stdio-only`** for stdio only, or **`--http-only`** for HTTP only. Run **`./start-mcp.sh --help`** for options.

To run HTTP only (no stdio), use `./start-mcp.sh --http-only [--port 4006]`. The HTTP server is implemented with **Hunchentoot**. It listens on `http://127.0.0.1:PORT`; clients POST JSON-RPC to `/rpc` or `/mcp`. GET endpoints include `/health`, `/lisply/ping-lisp`, `/lisply/tools/list`. Default port is 4006 (to avoid Swank on 4005). The process stays alive until you stop it with **Ctrl+C** or `./start-mcp.sh --stop`. **Kilocode Streamable HTTP:** use `"url": "http://127.0.0.1:4006/mcp"` (Tron serves MCP at both `/rpc` and `/mcp`).

### Kilocode: both transports, choose one

The project's **`.kilocode/mcp.json`** and **`examples/kilocode-mcp.json.example`** define **both** Tron variants so you can pick the one you want:

| Server name           | Transport       | Use case                                                                                    |
| --------------------- | --------------- | ------------------------------------------------------------------------------------------- |
| **cl-tron-mcp-stdio** | STDIO           | Default; client starts the server                                                           |
| **cl-tron-mcp-http**  | Streamable HTTP | You run `./start-mcp.sh` (combined) or `./start-mcp.sh --http-only`; client connects by URL |

**Choose one:** enable the server you want in Kilocode (set `disabled: false` for that entry); leave the other disabled or remove it. The different names are there so both options are available and you select which to use.

Tron follows [Kilo.ai MCP docs](https://kilo.ai/docs/automate/mcp/using-in-kilo-code) and [Server Transports](https://kilo.ai/docs/automate/mcp/server-transports):

- **STDIO (cl-tron-mcp-stdio):** JSON-RPC 2.0, one message per line. Use `command` (path to `start-mcp.sh`) and `args: ["--stdio-only"]`. No `type` or `url`.
- **Streamable HTTP (cl-tron-mcp-http):** Start Tron with `./start-mcp.sh --http-only [--port 4006]` (or default combined mode; HTTP is on 4006). Use `"type": "streamable-http"` and `"url": "http://127.0.0.1:4006/mcp"`. Tron serves MCP at both `/mcp` and `/rpc`.

### Lisp Selection

The script chooses the Lisp in this order:

1. **CLI:** `--use-sbcl` or `--use-ecl` — use that Lisp (error if not installed).
2. **Auto-detect:** try `sbcl`, then `ecl`; error if neither is found.

Example: `./start-mcp.sh --use-ecl` to force ECL when both are installed.

## One-Time Precompile (Avoid First-Start Timeout)

The first time the client starts the server, SBCL may compile the system, which can take 1–2 minutes. Many clients use a startup timeout (e.g. 30–60 seconds) and may report "failed to start" if the first JSON line does not appear in time.

**Do this once** before relying on the client to start the MCP:

```bash
cd ~/quicklisp/local-projects/cl-tron-mcp
sbcl --noinform --eval '(ql:quickload :cl-tron-mcp)' --eval '(quit)'
```

Adjust the path if your Quicklisp is elsewhere. After this, the next client start should be fast.

## Troubleshooting Checklist

If the MCP or Tron "doesn't start" or the client says the server failed:

1. **Check path in config** — Ensure the command uses the correct path. Tilde expansion (`~`) works in most MCP clients.
2. **Precompile once** — Run the one-time precompile command above so the first client start stays under the client's timeout.
3. **Lisp in PATH** — The client runs the command in its own environment. Ensure `sbcl` (or `ecl`) is on the PATH. To use ECL, pass **`--use-ecl`** in the command (e.g. `["~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh", "--use-ecl"]`).
4. **No stdout pollution** — Use `start-mcp.sh` (recommended) or, if you run Lisp directly, use SBCL with `--noinform` or ECL with `-q`. Any output on stdout before the first JSON line will break the MCP handshake.
5. **Port conflicts** — For HTTP mode, if port 4006 is in use (e.g. by Swank), use `--port <different-port>`.

## Debugging Kilocode MCP

Best workflow to isolate why Kilocode does not connect:

### 1. Prove Tron works alone (stdio)

From the project root, run:

```bash
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh --stdio-only 2>/dev/null | head -1
```

**Expected:** A single line starting with `{` (a JSON object). If you see anything else (banner, log line, or nothing), fix Tron first: precompile once, ensure no stdout before the JSON line.

Optional capture script (writes to `reports/mcp-stdio-out.log` and `reports/mcp-stdio-err.log`). **Precompile first** so the server responds within the script timeout:

```bash
sbcl --noinform --eval '(ql:quickload :cl-tron-mcp)' --eval '(quit)'
./scripts/debug-mcp-stdio.sh
cat reports/mcp-stdio-out.log   # should start with {
cat reports/mcp-stdio-err.log   # stderr from server
```

### 2. Run the exact command Kilocode uses

From your `.kilocode/mcp.json`, take the `command` and `args` and run them in a terminal. Example:

```bash
~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh --stdio-only
# In another terminal (or pipe): send one line, read one line
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh --stdio-only 2>/dev/null | head -1
```

If this fails (wrong path, permission, or no JSON), fix the path or environment (PATH for `sbcl`) before changing Kilocode config.

### 3. Use one transport at a time

- **STDIO (cl-tron-mcp-stdio):** Use `command` and `args: ["--stdio-only"]`. Do not set `type` or `url`. Set `disabled: false` for the stdio server.
- **Streamable HTTP (cl-tron-mcp-http):** Start Tron yourself: `./start-mcp.sh` (combined) or `./start-mcp.sh --http-only --port 4006`. Use `"type": "streamable-http"` and `"url": "http://127.0.0.1:4006/mcp"`. The config lists both so you choose which one to enable.

### 4. Check Kilocode / VS Code output

- In VS Code: **View > Output**, select **"Kilo Code"** or **"MCP"** (or the extension that runs MCP).
- Look for errors: "timeout", "failed to start", "invalid JSON", "connection refused". That tells you whether the problem is startup, timeout, or response format.

### 5. Increase timeout

Kilocode defaults to a 1-minute network timeout. First start (or slow load) can exceed it. In Kilocode MCP settings for this server, try **Network Timeout** 2–5 minutes.

### 6. Minimal config to try

Single server, stdio only, tilde path:

```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "command": "~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh",
      "args": ["--stdio-only"],
      "disabled": false
    }
  }
}
```

Enable the server in the Kilocode UI and restart. If it still fails, use step 1 and 4 to see whether Tron responds correctly and what Kilocode reports.

## See Also

- [AGENTS.md](../AGENTS.md) — Quick start for agents
- [README: Quick Start](../README.md#quick-start) — Client config snippets
- [examples/](../examples/) — Portable example configs with tilde expansion
