# Starting the MCP Server

This is the canonical install, startup, and verification guide for Tron.

## Recommended Model

Tron works best when you keep **one long-running Lisp session** alive and let Tron connect to it through Swank:

1. start SBCL,
2. start Swank inside that image,
3. start Tron,
4. connect Tron to Swank with `repl_connect` or `swank_connect`,
5. debug, inspect, compile, and continue inside the same session.

## Installation Paths

### Recommended: Quicklisp-first

Clone the project into Quicklisp local projects:

```bash
git clone https://github.com/Alba-Intelligence/cl-tron-mcp.git \
  ~/quicklisp/local-projects/cl-tron-mcp
cd ~/quicklisp/local-projects/cl-tron-mcp
```

If the repository lives somewhere else, make sure Quicklisp can still find it by either:

- symlinking it into `~/quicklisp/local-projects/`, or
- pushing its path into `ql:*local-project-directories*` before loading.

### Optional: devenv / Nix

`devenv.nix` is available for contributors who want a reproducible shell with SBCL, ECL, OpenSSL variants, and helper tooling already installed. It is optional and not required for ordinary Quicklisp-based use.

## First Load / Precompile

The first load can take long enough to hit MCP-client startup timeouts. Preload once from the repository root:

```bash
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(ql:quickload :cl-tron-mcp :silent t)'
```

## Start the Target Lisp Session

In the Lisp image you want Tron to control:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

### Port Guidance

- Use **4006** for the Swank session shown in these examples.
- If your editor already owns another Swank port, keep that separate and point Tron at the dedicated port you want it to use.

## Start Tron

From the repository root, `start-mcp.sh` is the primary runtime entrypoint:

```bash
./start-mcp.sh --stdio-only
./start-mcp.sh
./start-mcp.sh --http-only --port 9000
./start-mcp.sh --use-sbcl
./start-mcp.sh --use-ecl
```

### Transport Modes

| Mode | Command | Lifecycle | Use when |
| --- | --- | --- | --- |
| `stdio-only` | `./start-mcp.sh --stdio-only` | exits with the client connection | the MCP client launches Tron directly |
| `combined` | `./start-mcp.sh` | long-running | you want HTTP available and a persistent server process |
| `http-only` | `./start-mcp.sh --http-only` | long-running | you only need HTTP transport |

In long-running modes, the shell launcher supervises the Lisp process. `Ctrl+C` cleanly stops the child process and removes the PID file instead of dropping ECL into its interactive debugger.

### Optional Wrapper: `run-mcp.sh`

`run-mcp.sh` enters `devenv shell` and then forwards to `start-mcp.sh`. Use it only if you explicitly want the Nix/devenv environment.

## Verify Startup

### Verify the stdio handshake

```bash
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | \
  ./start-mcp.sh --stdio-only 2>/dev/null | head -1
```

Expected: one JSON-RPC line with `result.serverInfo`.

### Verify HTTP health

```bash
./start-mcp.sh
curl http://127.0.0.1:4006/health
```

Expected: a JSON response containing an `ok` status.

### Stop a long-running server

```bash
./start-mcp.sh --stop
```

Or, if you started the server in the foreground, press `Ctrl+C`.

## Connect Tron to Swank

After the MCP server is running, connect it to the live Lisp session:

```json
{
  "name": "repl_connect",
  "arguments": { "port": 4006 }
}
```

If you need the lower-level API:

```json
{
  "name": "swank_connect",
  "arguments": { "port": 4006 }
}
```

## MCP Client Configuration

Point the client at the repository's `start-mcp.sh` script. A common path is:

```text
~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh
```

Examples for several clients live under [`examples/`](../examples/). The helper script [`create_configs.sh`](../create_configs.sh) can generate config files with absolute paths.

## Troubleshooting

### Quicklisp not found

Install Quicklisp or set `QUICKLISP_DIR` if your setup lives somewhere other than `~/quicklisp`.

### First start is too slow

Run the one-time preload command from the **First Load / Precompile** section.

### Stdout is polluted

For MCP stdio mode, use `start-mcp.sh`. It configures Lisp startup flags so the client sees JSON-RPC on stdout instead of banners or logs.

### Port conflict

If 4006 is already in use, choose a different port consistently for:

1. the Swank session,
2. the MCP client connection arguments, and
3. any HTTP listener you start.

### Ctrl+C does not stop the server

Use the current `start-mcp.sh` from this repository root. Long-running modes are expected to stop cleanly on `Ctrl+C`; if you still have an orphaned process, run `./start-mcp.sh --stop` or `./start-mcp.sh --stop --force`.

### devenv-specific issues

If you intentionally use `run-mcp.sh` and hit missing-library or shell-environment problems, debug the `devenv` layer separately. Those are optional-environment issues, not required parts of the normal Quicklisp setup.

## Related Docs

- [../README.md](../README.md) - project overview and shortest path
- [architecture.md](architecture.md) - one-session runtime model
- [code-reference.md](code-reference.md) - source-level map
- [tools/index.md](tools/index.md) - full tool catalog
