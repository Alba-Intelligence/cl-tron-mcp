;;;; discover-mcp.lisp
;;;; Prompt: How to Fully Use This MCP (No User Explanation Needed)

(in-package :cl-tron-mcp/prompts)

(define-prompt
    "discover-mcp"
  "How to Fully Use This MCP (No User Explanation Needed)"
  "Exact steps for an AI agent to learn and use the MCP without any user explanation. Call this first if you do not know how the MCP works."
  nil
  (list
   (list :role "user"
         :content
         (list :type "text"
               :text
               "How can I learn to fully use this MCP without a user having to explain anything?

Call these MCP methods in order. After that you have everything needed to connect, evaluate, debug, inspect, profile, and hot-reload.

## Step 1: List documentation

**resources/list** — Returns URIs of all documentation files.

## Step 2: Read the agent guide

**resources/read** with param `uri` = `AGENTS.md` (or `file://AGENTS.md`).

That document explains: what Tron is, the one long-running Lisp session pattern, connection, tool categories, workflows, troubleshooting, and JSON/key conventions.

## Step 3: List guided workflows

**prompts/list** — Returns names and descriptions of workflows.

## Step 4: Get connection steps

**prompts/get** with param `name` = `getting-started`.

Returns step-by-step instructions to check/start Swank, connect, and verify.

## Step 5: List all tools

**tools/list** — Returns every tool with name, description, and input schema (parameters). Use this as the full tool reference.

## Summary

| Order | Method         | Purpose                          |
|-------|----------------|----------------------------------|
| 1     | resources/list | Documentation URIs               |
| 2     | resources/read | Read AGENTS.md (full guide)      |
| 3     | prompts/list   | Workflow names                   |
| 4     | prompts/get    | getting-started (connection)     |
| 5     | tools/list     | All tools and parameters         |

No user explanation is required. This MCP is fully discoverable."))))
