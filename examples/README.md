# Example MCP Configurations

Copy the example that matches your MCP client. All examples use **tilde expansion** (`~`) for the standard Quicklisp path:

```
~/quicklisp/local-projects/cl-tron-mcp/start-mcp.sh
```

MCP clients (Cursor, VS Code, Kilocode) support tilde expansion but do not support `$HOME` or other environment variables. Adjust the path if your Quicklisp is in a different location.

| File | Client | Config location |
|------|--------|------------------|
| `cursor-mcp.json.example` | Cursor | `~/.cursor/mcp.json` (or Cursor MCP settings) |
| `opencode-mcp.json.example` | OpenCode | `~/.config/opencode/opencode.json` |
| `kilocode-mcp.json.example` | Kilocode | `~/.kilocode/cli/config.json` (or equivalent) |
| `mcp-kilocode.json` | Kilocode | Alternative Kilocode config |

**Example clients:** `test_client.py` and `cl_tron_client.py` are minimal Python clients for testing MCP (e.g. run from repo root with `PYTHONPATH=examples python examples/test_client.py`).

All examples use `start-mcp.sh` so stdout stays clean for the MCP protocol. To force SBCL or ECL, add `--use-sbcl` or `--use-ecl` to the command. Run `./start-mcp.sh --help` for full usage.

## Server Management

For HTTP and combined modes, the script detects if a server is already running:

```bash
./start-mcp.sh --status   # Check if server is running
./start-mcp.sh --stop     # Stop a running HTTP server
```

See [docs/starting-the-mcp.md](../docs/starting-the-mcp.md) for troubleshooting.
