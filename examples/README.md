# Example MCP Configurations

Copy the example that matches your MCP client and **replace `/path/to/cl-tron-mcp`** with the actual path to your `cl-tron-mcp` directory.

| File | Client | Config location |
|------|--------|------------------|
| `cursor-mcp.json.example` | Cursor | `~/.cursor/mcp.json` (or Cursor MCP settings) |
| `opencode-mcp.json.example` | OpenCode | `~/.config/opencode/opencode.json` |
| `kilocode-mcp.json.example` | Kilocode | `~/.kilocode/cli/config.json` (or equivalent) |
| `mcp-kilocode.json` | Kilocode | Alternative Kilocode config (replace path) |

**Example clients:** `test_client.py` and `cl_tron_client.py` are minimal Python clients for testing MCP (e.g. run from repo root with `PYTHONPATH=examples python examples/test_client.py`).

All examples use `start-mcp.sh` so stdout stays clean for the MCP protocol. To force SBCL or ECL, add `--use-sbcl` or `--use-ecl` to the command. Run `./start-mcp.sh --help` for full usage. See [docs/starting-the-mcp.md](../docs/starting-the-mcp.md) for troubleshooting.
