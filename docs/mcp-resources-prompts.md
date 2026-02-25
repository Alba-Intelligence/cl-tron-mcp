# MCP Resources and Prompts

This document describes the MCP Resources and Prompts features added to cl-tron-mcp to improve discoverability for AI agents.

## Overview

### Problem

AI agents using cl-tron-mcp didn't know how to use the tools because:

1. They didn't know about `swank_connect` / `repl_connect`
2. They didn't understand the workflow (connect first, then evaluate)
3. Error messages didn't provide actionable guidance

### Solution

1. **MCP Resources** - Expose documentation files that agents can discover
2. **MCP Prompts** - Provide guided workflows as slash commands
3. **Improved Tool Descriptions** - Include prerequisites in tool descriptions
4. **Better Error Messages** - Include hints for common issues

## MCP Resources

Resources are documentation files exposed via the MCP `resources/list` and `resources/read` protocol methods.

### Protocol Methods

#### resources/list

Returns list of available documentation files.

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "resources/list"
}
```

Response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "resources": [
      {
        "uri": "file://AGENTS.md",
        "name": "AGENTS.md",
        "title": "Documentation: AGENTS.md",
        "description": "MCP documentation file: AGENTS.md",
        "mimeType": "text/markdown",
        "size": 12345
      }
    ]
  }
}
```

#### resources/read

Read contents of a specific documentation file.

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "resources/read",
  "params": {
    "uri": "file://AGENTS.md"
  }
}
```

Response:

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "contents": [
      {
        "uri": "file://AGENTS.md",
        "mimeType": "text/markdown",
        "text": "# SBCL Debugging MCP Repository Guidelines..."
      }
    ]
  }
}
```

### Exposed Resources

The following files are exposed as MCP resources:

- `AGENTS.md` - Quick start guide for AI agents
- `README.md` - Project overview
- `docs/tools/debugger.md` - Debugger tool documentation
- `docs/tools/inspector.md` - Inspector tool documentation
- `docs/tools/hot-reload.md` - Hot reload documentation
- `docs/tools/profiler.md` - Profiler documentation
- `docs/tools/threads.md` - Thread management documentation
- `docs/tools/monitor.md` - Monitor documentation
- `docs/architecture.md` - System architecture
- `docs/starting-the-mcp.md` - Starting the MCP and troubleshooting
- `docs/swank-integration.md` - Swank protocol details
- `tutorial/e2e-mcp-workflow.md` - End-to-end workflow with unified `repl_*` tools

### Security

- Only whitelisted files are exposed
- Path traversal is prevented
- No sensitive files (`.env`, `secrets/`, credentials) are exposed

## MCP Prompts

Prompts are guided workflows exposed via the MCP `prompts/list` and `prompts/get` protocol methods. They appear as slash commands in MCP clients.

### Protocol Methods

#### prompts/list

Returns list of available guided workflows.

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "prompts/list"
}
```

Response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "prompts": [
      {
        "name": "discover-mcp",
        "title": "How to Fully Use This MCP (No User Explanation Needed)",
        "description": "Exact steps to learn and use the MCP without user explanation."
      },
      {
        "name": "getting-started",
        "title": "Getting Started with Tron MCP",
        "description": "Step-by-step guide to connect to Swank and verify..."
      }
    ]
  }
}
```

#### prompts/get

Get a specific workflow with step-by-step instructions.

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "prompts/get",
  "params": {
    "name": "getting-started"
  }
}
```

Response format (MCP spec): `result.messages` is an array of messages; each message has `role` (e.g. `"user"`) and `content` as an **array of parts** (e.g. `[{ "type": "text", "text": "..." }]`), not a single content object. Part keys are lowercase. This structure is required for Cursor and other MCP clients that use prompts as slash commands.

### Available Prompts

#### discover-mcp

**Call this first if you do not know how the MCP works.** Returns the exact ordered steps for an AI agent to fully learn and use the MCP without any user explanation: resources/list → resources/read AGENTS.md → prompts/list → prompts/get getting-started → tools/list. The MCP is fully discoverable via these standard methods.

#### getting-started

Step-by-step guide to:

1. Check if Swank is running
2. Start Swank server if needed
3. Connect using `swank_connect` or `repl_connect`
4. Verify connection with `(+ 1 2)`
5. Understand the persistent session model

#### debugging-workflow

Step-by-step process for:

1. Triggering an error
2. Getting the backtrace
3. Getting available restarts
4. Fixing the code
5. Verifying the fix

#### hot-reload-workflow

Guide for:

1. Identifying bugs
2. Fixing source files
3. Reloading code without restart
4. Verifying fixes
5. Committing changes

#### profiling-workflow

Performance analysis workflow:

1. Start profiling
2. Run code
3. Stop profiling
4. Analyze results
5. Optimize hotspots

## Server-enforced approval

Tools with `approvalLevel: "user"` require human approval before they run. The flow uses a dedicated response shape and method so clients can show UI and re-invoke the tool.

### approval_required response

When a tool with `approvalLevel: "user"` is invoked and the operation is not whitelisted, `tools/call` does not run the tool. Instead it returns a **result** whose `content[0].type` is `"text"` and `content[0].text` is JSON with:

- `approval_required: true`
- `request_id` (string) – use this when calling `approval/respond`
- `message` (string) – short text for the user (e.g. "User approval required for tool: repl_eval")

The client should show this to the user and offer Approve / Deny (and optionally Retry later).

### approval/respond method

After the user decides, the client sends:

- **Method:** `approval/respond`
- **Params:** `request_id` (string), `approved` (boolean), optional `message` (string, e.g. denial reason)
- **Response:** `result.recorded` (boolean), `result.approved` (boolean); on denial, `result.message` (string)

If the user approved, the client **re-invokes** the same `tools/call` with the same tool name and arguments, plus `approval_request_id` (the same `request_id`) and `approved: true`. The server then runs the tool and returns the normal result. Retry after a denial means calling the same tool again (a new approval request is created). For full flow (300s timeout, two classes user/none, whitelist), see [AGENTS.md](../AGENTS.md#approval-workflow-server-enforced).

## Capability Declaration

The server now declares these capabilities during initialization:

```json
{
  "capabilities": {
    "tools": { "listChanged": true },
    "resources": { "subscribe": false, "listChanged": true },
    "prompts": { "listChanged": true }
  }
}
```

## Error Messages with Hints

Error messages now include actionable hints:

**Before:**

```json
{
  "error": true,
  "message": "Not connected to any REPL"
}
```

**After:**

```json
{
  "error": true,
  "message": "Not connected to any REPL",
  "hint": "Run repl_connect first. Example: repl_connect :port 4006",
  "setup": "To start Swank in SBCL: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)",
  "docs": "See prompts/get 'getting-started' for step-by-step instructions"
}
```

## Implementation Files

| File                            | Purpose                                 |
| ------------------------------- | --------------------------------------- |
| `src/resources/package.lisp`    | Resources package definition            |
| `src/resources/handler.lisp`    | Resource listing and reading            |
| `src/prompts/package.lisp`      | Prompts package definition              |
| `src/prompts/handler.lisp`      | Prompt listing and retrieval            |
| `src/protocol/handlers.lisp`    | Protocol handlers for resources/prompts |
| `src/tools/register-tools.lisp` | Updated tool descriptions               |
| `src/unified/client.lisp`       | Error messages with hints               |

## Testing

Test resources:

```bash
echo '{"jsonrpc":"2.0","method":"resources/list","id":1}' | ./start-mcp.sh
echo '{"jsonrpc":"2.0","method":"resources/read","params":{"uri":"file://AGENTS.md"},"id":2}' | ./start-mcp.sh
```

Test prompts:

```bash
echo '{"jsonrpc":"2.0","method":"prompts/list","id":1}' | ./start-mcp.sh
echo '{"jsonrpc":"2.0","method":"prompts/get","params":{"name":"getting-started"},"id":2}' | ./start-mcp.sh
```

Test error hints:

```bash
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"repl_eval","arguments":{"code":"(+ 1 2)"}},"id":1}' | ./start-mcp.sh
```

Should return error with hint about connecting first.
