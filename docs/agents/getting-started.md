# Getting Started

## How to Use This MCP (Fully Discoverable — No User Explanation Needed)

An AI agent can learn how to fully use this MCP without any user explanation. Use one of these entry points:

- **If you prefer a short recipe:** Call **prompts/get** with name **`discover-mcp`**. It returns the exact ordered steps (resources/list → resources/read AGENTS.md → prompts/list → prompts/get getting-started → tools/list).
- **If you prefer to read:** Call **resources/list**, then **resources/read** with uri **`AGENTS.md`**. This document (plus the other listed resources) explains connection, tools, workflows, and conventions.

After following that path you have everything needed to connect to Swank, evaluate code, debug, inspect, profile, and hot-reload. The MCP is fully discoverable via standard MCP methods: `resources/list`, `resources/read`, `prompts/list`, `prompts/get`, `tools/list`.

## What is Tron?

Tron is an MCP server that connects to a running SBCL Lisp session and provides debugging, code evaluation, inspection, profiling, and hot-reload capabilities.

### Discovering How to Use Tron

Tron exposes documentation and guided workflows via MCP standard mechanisms:

| Method           | Purpose                                      |
| ---------------- | -------------------------------------------- |
| `resources/list` | List available documentation files           |
| `resources/read` | Read a documentation file by URI             |
| `prompts/list`   | List available guided workflows              |
| `prompts/get`    | Get step-by-step instructions for a workflow |

**Recommended:** Start with `prompts/get` for `getting-started` workflow to learn the connection pattern.

## Essential Pattern: One Long-Running Session

```
┌─────────────────┐         ┌─────────────────┐
│  SBCL + Swank   │◄───────►│   Tron (MCP)    │
│  (Port 4006)    │         │   (stdio)       │
│                 │         │                 │
│  Your code      │         │   AI Agent      │
│  Debugger state │         │   evaluates     │
│  Threads        │         │   inspects      │
└─────────────────┘         └─────────────────┘
```

**Never restart the SBCL session.** Tron connects as a client; all state lives in SBCL.

## First Steps

We recommend using port 4006 for the Swank server. That way 4005 is available for the user's Slime.

### Starting Swank in SBCL

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

### Connecting from Tron

Use `repl_connect` or `swank_connect` to connect to the running Swank server.

## Key Insights

- **All tools use `&key` args with underscore names**: `symbol_name`, not `symbol-name`
- **Results are plists**: Access with `(getf result :key)`
- **Port 4005 = Swank** (Slime/Portacle/Sly)
- **Use `tmp/` folder**, never `/tmp`

## Lisp Implementation Support

- **Primary:** Tested with **SBCL**; Swank integration and debugger features are developed against SBCL.
- **ECL:** The MCP server can run under **ECL** as well. Lisp selection in `start-mcp.sh`: **CLI** (`--use-sbcl` / `--use-ecl`) or **auto-detect** (sbcl, then ecl). Run `./start-mcp.sh --help` for full usage. For stdio, the script uses SBCL `--noinform` or ECL `-q` so stdout stays JSON-only.
- **Goal:** Any Common Lisp implementation should be able to use the MCP where possible; REPL connectivity is via Swank.

## MCP Protocol Methods

| Method           | Purpose                                                  |
| ---------------- | -------------------------------------------------------- |
| `resources/list` | List documentation files (AGENTS.md, README.md, docs/\*) |
| `resources/read` | Read a documentation file by URI                         |
| `prompts/list`   | List guided workflows                                    |
| `prompts/get`    | Get step-by-step workflow instructions                   |
| `tools/list`     | List available tools                                     |
| `tools/call`     | Invoke a tool                                            |

## Guided Workflows (prompts/get)

| Prompt                | Purpose                                                                      |
| --------------------- | ---------------------------------------------------------------------------- |
| `discover-mcp`        | How to fully use this MCP without user explanation (ordered discovery steps) |
| `getting-started`     | How to connect to Swank and verify setup                                     |
| `debugging-workflow`  | Step-by-step error debugging                                                 |
| `hot-reload-workflow` | Live code modification without restart                                       |
| `profiling-workflow`  | Performance analysis workflow                                                |