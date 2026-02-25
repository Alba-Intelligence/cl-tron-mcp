# Starting the MCP Server

This document explains how the MCP server is started and how to fix common "MCP won't start" issues.

## Who Starts the MCP

- **The MCP client** (Cursor, OpenCode, Kilocode) starts the server when you add it to your config and (re)start the client. The client runs the configured command (e.g. `start-mcp.sh`).
- **The AI agent** only uses tools _after_ the server is running. The agent does not start the server; if the server fails to start, the cause is client config or environment.

## Transport Modes

| Mode | Command | Lifecycle | Use Case |
|------|---------|-----------|----------|
| **stdio-only** | `--stdio-only` | Short-lived, exits when client disconnects | MCP client starts the server |
| **http-only** | `--http-only` | Long-running until stopped | Manual start, multiple clients |
| **combined** | (default) | Long-running HTTP server | Recommended for IDE sessions |

### stdio-only Mode

Short-lived process for MCP client communication. The process exits when the client closes the connection.

```bash
./start-mcp.sh --stdio-only
```

### HTTP / Combined Mode (Recommended)

Long-running HTTP server that stays alive across client sessions. This is the **recommended mode** for IDE usage.

```bash
./start-mcp.sh                    # Combined mode (long-running HTTP on 4006)
./start-mcp.sh --http-only        # Same as default
./start-mcp.sh --port 9000        # Use a different port
```

For MCP clients, configure **streamable HTTP** transport:
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "type": "streamable-http",
      "url": "http://127.0.0.1:4006/mcp"
    }
  }
}
```

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

## Server Detection and Session Management

For HTTP and combined modes, `start-mcp.sh` includes automatic server detection:

### PID File

The PID file (`.tron-server.pid`) contains JSON metadata:

```json
{
  "pid": 12345,
  "port": 4006,
  "transport": "combined",
  "started": 1709000000
}
```

### Status Commands

```bash
./start-mcp.sh --status   # Check if server is running
./start-mcp.sh --stop     # Stop a running server
./start-mcp.sh --restart  # Stop existing and start new
```

### Example Output

```
$ ./start-mcp.sh --status
Server is RUNNING
  PID: 12345
  Port: 4006
  Transport: combined
  Started: Tue Feb 27 10:30:00 UTC 2026
```

### Automatic Detection

When starting the server:
1. Checks if a server is already running (via PID file + health endpoint)
2. If healthy server exists → exits successfully (no duplicate instance)
3. If unhealthy → offers to restart
4. If port in use by another process → exits with error

## Kilocode Configuration

The project's **`.kilocode/mcp.json`** provides both transport options:

| Server name           | Transport        | Use when...                          |
|-----------------------|------------------|--------------------------------------|
| **cl-tron-mcp-stdio** | STDIO            | You want Kilocode to start Tron each session |
| **cl-tron-mcp-http**  | Streamable HTTP  | You run Tron manually (recommended)  |

**Recommended approach:**

1. Start Tron once: `./start-mcp.sh`
2. Configure Kilocode for streamable HTTP:
```json
{
  "mcpServers": {
    "cl-tron-mcp": {
      "type": "streamable-http",
      "url": "http://127.0.0.1:4006/mcp",
      "disabled": false
    }
  }
}
```

This keeps Tron running across Kilocode sessions without repeated startup.

## Lisp Selection

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
3. **Lisp in PATH** — The client runs the command in its own environment. Ensure `sbcl` (or `ecl`) is on the PATH. To use ECL, pass **`--use-ecl`** in the command.
4. **No stdout pollution** — Use `start-mcp.sh` (recommended) or, if you run Lisp directly, use SBCL with `--noinform` or ECL with `-q`. Any output on stdout before the first JSON line will break the MCP handshake.
5. **Port conflicts** — If port 4006 is in use (e.g. by Swank), use `--port <different-port>`.
6. **Check server status** — Run `./start-mcp.sh --status` to see if a server is already running.

## Debugging Kilocode MCP

Best workflow to isolate why Kilocode does not connect:

### 1. Prove Tron works alone (stdio)

From the project root, run:

```bash
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | ./start-mcp.sh --stdio-only 2>/dev/null | head -1
```

**Expected:** A single line starting with `{` (a JSON object). If you see anything else (banner, log line, or nothing), fix Tron first: precompile once, ensure no stdout before the JSON line.

### 2. Use streamable HTTP (recommended)

For a persistent server:

```bash
# Start Tron
./start-mcp.sh

# Verify it's running
./start-mcp.sh --status

# Configure Kilocode for HTTP
# "url": "http://127.0.0.1:4006/mcp"
```

### 3. Check Kilocode / VS Code output

- In VS Code: **View > Output**, select **"Kilo Code"** or **"MCP"**.
- Look for errors: "timeout", "failed to start", "invalid JSON", "connection refused".

### 4. Increase timeout

Kilocode defaults to a 1-minute network timeout. First start can exceed it. In Kilocode settings, try **Network Timeout** 2–5 minutes.

## See Also

- [AGENTS.md](../AGENTS.md) — Quick start for agents
- [README: Quick Start](../README.md#quick-start) — Client config snippets
- [examples/](../examples/) — Portable example configs with tilde expansion
