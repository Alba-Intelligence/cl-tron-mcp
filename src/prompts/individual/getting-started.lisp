;;;; getting-started.lisp
;;;; Prompt: Getting Started with Tron MCP

(in-package :cl-tron-mcp/prompts)

(define-prompt
    "getting-started"
  "Getting Started with Tron MCP"
  "Step-by-step guide to connect to Swank and verify the MCP server is working correctly."
  nil
  (list
   (list :role "user"
         :content
         (list :type "text"
               :text
               "I want to use Tron MCP for Common Lisp development. How do I get started?

Follow these steps:

## Step 1: Check if Swank is Running

First, check if a Swank server is already running:

```bash
ss -tlnp | grep 4006
```

If you see output, Swank is running. Skip to Step 3.

## Step 2: Start Swank Server

If Swank is not running, start it in your SBCL session:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

This creates a persistent Swank server that Tron can connect to.

## Step 3: Connect to Swank

Use the swank_connect tool:

```
swank_connect :host \"127.0.0.1\" :port 4006
```

Or use the unified interface (Swank):

```
repl_connect :port 4006
```

## Step 4: Verify Connection

Test the connection with a simple expression:

```
swank_eval :code \"(+ 1 2)\"
```

Expected result: \"3\"

## Step 5: You're Ready!

You now have a persistent Lisp session. You can:
- Evaluate code with swank_eval
- Debug errors with swank_backtrace and swank_get_restarts
- Hot-reload code with swank_compile
- Profile performance with profile_start/stop

## If the MCP or Tron Doesn't Start

The MCP server is started by the **client** (Cursor, OpenCode, Kilocode), not by the agent. If the user says the MCP or Tron doesn't start:

1. **Check client config** — Ensure the command uses the correct path to cl-tron-mcp and uses `start-mcp.sh` (or SBCL with `--noinform`). See docs/starting-the-mcp.md.
2. **One-time precompile** — Suggest running once: `cd ~/quicklisp/local-projects/cl-tron-mcp && sbcl --noinform --eval '(ql:quickload :cl-tron-mcp)' --eval '(quit)'` so the first client start stays under the client's timeout.
3. **SBCL in PATH** — Ensure sbcl is on the PATH when the client starts the server. Check client logs for errors.

## Important Notes

- **Never restart the SBCL session** - All state lives there
- **Tron is a client** - It connects to your running SBCL
- **Session persists** - Variables, functions, and state remain across calls
- **Use resources/list** to see available documentation"))))
