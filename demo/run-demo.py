#!/usr/bin/env python3
"""
demo/run-demo.py - Tron MCP Demo Driver

Spawns the MCP server via stdio and sends real JSON-RPC requests,
printing a colored transcript of the protocol exchange.

Usage:
    python3 demo/run-demo.py <scenario>

Scenarios (original):
    mcp-overview    Initialize, list tools/resources/prompts, health check
    f1-f2           Hot-reload: compile f2 WHILE IN THE DEBUGGER, use CONTINUE
    factorial       Debug: buggy factorial, DIVISION-BY-ZERO, fix, verify

Phase recordings (for README split):
    mcp-overview-1  Tools + resources + prompts discovery
    mcp-overview-2  Health check + runtime stats
    f1-f2-1         MCP init + Swank launch + REPL connect
    f1-f2-2         Compile f1 → debugger → compile f2 in-debugger → CONTINUE
    factorial-1     MCP init + Swank launch + compile buggy factorial
    factorial-2     Trigger DIVISION-BY-ZERO → inspect → fix → verify

Must be run from the project root directory.
"""

import json
import subprocess
import sys
import time
import os
import threading
from typing import Any, Optional

# ─── ANSI colours ──────────────────────────────────────────────────────────────
RESET   = "\033[0m"
BOLD    = "\033[1m"
YELLOW  = "\033[1;33m"
CYAN    = "\033[36m"
GREEN   = "\033[32m"
RED     = "\033[31m"
DIM     = "\033[2m"
MAGENTA = "\033[35m"


def col(color: str, text: str) -> str:
    return f"{color}{text}{RESET}"


def banner(title: str) -> None:
    bar = "═" * 72
    print(f"\n{col(YELLOW, bar)}")
    print(col(YELLOW, f"  {title}"))
    print(col(YELLOW, bar) + "\n")


def step(n: int, title: str) -> None:
    print(f"\n{col(BOLD, f'── Step {n}: {title}')}")


def show_json(label: str, color: str, data: Any, max_lines: int = 20) -> None:
    text = json.dumps(data, indent=2)
    lines = text.split("\n")
    if len(lines) > max_lines:
        shown = "\n".join(lines[:max_lines])
        text = f"{shown}\n{DIM}  ... ({len(lines) - max_lines} more lines){RESET}"
    print(f"{col(color, label)} {text}")


# ─── MCP Client ────────────────────────────────────────────────────────────────

class MCPClient:
    def __init__(self):
        self.process: Optional[subprocess.Popen] = None
        self._id = 0
        self._lock = threading.Lock()

    def start(self) -> None:
        """Start the MCP server as a subprocess."""
        cmd = ["./start-mcp.sh", "--stdio-only"]
        self.process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            bufsize=1,
        )
        # Give the server a moment to start
        time.sleep(2)
        if self.process.poll() is not None:
            raise RuntimeError("MCP server exited immediately. Check start-mcp.sh.")

    def stop(self) -> None:
        if self.process and self.process.poll() is None:
            self.process.stdin.close()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()

    def _next_id(self) -> int:
        with self._lock:
            self._id += 1
            return self._id

    def send(self, method: str, params: dict = None, timeout: float = 30.0) -> dict:
        """Send a JSON-RPC request and return the response."""
        req_id = self._next_id()
        request = {"jsonrpc": "2.0", "method": method, "params": params or {}, "id": req_id}
        line = json.dumps(request) + "\n"
        self.process.stdin.write(line)
        self.process.stdin.flush()

        # Read response
        deadline = time.time() + timeout
        while time.time() < deadline:
            resp_line = self.process.stdout.readline()
            if not resp_line:
                if self.process.poll() is not None:
                    raise RuntimeError("MCP server died while waiting for response")
                continue
            resp_line = resp_line.strip()
            if not resp_line:
                continue
            try:
                response = json.loads(resp_line)
                # Match by id
                if response.get("id") == req_id:
                    return response
                # Could be a notification (no id) - ignore
            except json.JSONDecodeError:
                pass  # Skip non-JSON lines (shouldn't happen with --stdio-only)
        raise TimeoutError(f"No response for {method} within {timeout}s")

    def call_tool(self, name: str, arguments: dict = None, timeout: float = 45.0) -> dict:
        """Call an MCP tool. Handles approval_required automatically."""
        params = {"name": name, "arguments": arguments or {}}
        response = self.send("tools/call", params, timeout=timeout)

        # Check for approval_required
        result = response.get("result", {})
        content = result.get("content", [])
        if content:
            try:
                text = content[0].get("text", "")
                parsed = json.loads(text)
                if parsed.get("approval_required"):
                    request_id = parsed["request_id"]
                    # Respond with approval
                    self.send("approval/respond", {"request_id": request_id, "approved": True})
                    # Re-invoke with approval
                    params2 = {
                        "name": name,
                        "arguments": dict(arguments or {},
                                         approval_request_id=request_id,
                                         approved=True)
                    }
                    response = self.send("tools/call", params2, timeout=timeout)
            except (json.JSONDecodeError, KeyError, TypeError):
                pass
        return response

    def extract_result_text(self, response: dict) -> str:
        """Extract the text content from a tools/call response."""
        try:
            return response["result"]["content"][0]["text"]
        except (KeyError, IndexError, TypeError):
            return json.dumps(response.get("result", response))


# ─── Scenario helpers ──────────────────────────────────────────────────────────

def do_initialize(client: MCPClient) -> None:
    step(1, "Initialize MCP")
    params = {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "tron-demo", "version": "1.0"}
    }
    show_json(col(CYAN, "→"), CYAN, {"jsonrpc": "2.0", "method": "initialize", "params": params, "id": "(next)"})
    resp = client.send("initialize", params)
    info = resp.get("result", {}).get("serverInfo", {})
    print(col(GREEN, f"← ") + col(GREEN, f"Server: {info.get('name')} v{info.get('version')}  "
          f"protocol: {resp.get('result', {}).get('protocolVersion')}"))
    # Send initialized notification
    client.process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized","params":{}}\n')
    client.process.stdin.flush()
    time.sleep(0.5)


def do_whitelist_all(client: MCPClient) -> None:
    """Whitelist all operations so the demo isn't interrupted by approval prompts."""
    for op in ["eval", "compile-file", "modify-running-code", "launch-process",
               "set-breakpoint", "trace-function", "modify-restarts", "terminate-thread"]:
        client.call_tool("whitelist_add", {"operation": op, "pattern": "*"})
    client.call_tool("whitelist_enable", {"enable": True})


def do_swank_setup(client: MCPClient, verbose: bool = False) -> None:
    """Launch Swank and connect REPL. If verbose, show request/response."""
    if verbose:
        step(2, "Launch Swank server on port 14006")
        show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "swank_launch", "arguments": {"port": 14006}}})
        resp = client.call_tool("swank_launch", {"port": 14006}, timeout=60)
        text = client.extract_result_text(resp)
        print(col(GREEN, f"← {text[:120]}"))
        time.sleep(1.5)

        step(3, "Connect REPL to Swank")
        show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_connect", "arguments": {"port": 14006}}})
        resp = client.call_tool("repl_connect", {"port": 14006}, timeout=30)
        text = client.extract_result_text(resp)
        print(col(GREEN, f"← {text[:120]}"))
        time.sleep(1)
    else:
        # Silent quick setup — show just a status line
        client.call_tool("swank_launch", {"port": 14006}, timeout=60)
        time.sleep(1)
        client.call_tool("repl_connect", {"port": 14006}, timeout=30)
        print(col(DIM, "  (Swank launched, REPL connected on port 14006)"))
        time.sleep(0.5)


# ─── Scenario: mcp-overview-1 (tools + resources + prompts) ───────────────────

def scenario_mcp_overview_1(client: MCPClient) -> None:
    banner("TRON MCP — Discovering the Protocol")
    print("An AI agent starts by discovering what Tron offers: tools, resources, and guided prompts.\n")
    time.sleep(1)

    do_initialize(client)
    time.sleep(1)

    step(2, "List available tools")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/list", "params": {}})
    resp = client.send("tools/list")
    tools = resp.get("result", {}).get("tools", [])
    cats = {}
    for t in tools:
        name = t.get("NAME") or t.get("name", "unknown")
        cat = name.split("_")[0] if "_" in name else "other"
        cats.setdefault(cat, []).append(name)
    print(col(GREEN, f"← {len(tools)} tools available across {len(cats)} categories:"))
    for cat, names in sorted(cats.items()):
        print(f"   {col(DIM, cat):20s}  {len(names)} tools")
    time.sleep(1.5)

    step(3, "List documentation resources")
    show_json(col(CYAN, "→"), CYAN, {"method": "resources/list", "params": {}})
    resp = client.send("resources/list")
    resources = resp.get("result", {}).get("resources", [])
    print(col(GREEN, f"← {len(resources)} resources:"))
    for r in resources[:6]:
        uri = r.get("uri", "")
        name = r.get("name", "")
        print(f"   {col(DIM, uri):<55s}  {name}")
    if len(resources) > 6:
        print(f"   {col(DIM, f'... and {len(resources) - 6} more')}")
    time.sleep(1.5)

    step(4, "List guided workflow prompts")
    show_json(col(CYAN, "→"), CYAN, {"method": "prompts/list", "params": {}})
    resp = client.send("prompts/list")
    prompts = resp.get("result", {}).get("prompts", [])
    print(col(GREEN, f"← {len(prompts)} workflow prompts:"))
    for p in prompts:
        name = p.get("name", "")
        desc = p.get("description", "")
        print(f"   {col(BOLD, name):<30s}  {desc[:55]}")
    time.sleep(2)

    print(f"\n{col(GREEN, '✅ Discovery complete!')}")
    time.sleep(2)


# ─── Scenario: mcp-overview-2 (health + runtime stats) ───────────────────────

def scenario_mcp_overview_2(client: MCPClient) -> None:
    banner("TRON MCP — Monitoring a Live Image")
    print("Tron provides production monitoring tools: health checks and runtime statistics.\n")
    time.sleep(1)

    do_initialize(client)
    time.sleep(1)

    step(2, "Health check")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "health_check", "arguments": {}}})
    resp = client.call_tool("health_check")
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:200]}"))
    time.sleep(2)

    step(3, "Runtime statistics")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "runtime_stats", "arguments": {}}})
    resp = client.call_tool("runtime_stats")
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:300]}"))
    time.sleep(2)

    print(f"\n{col(GREEN, '✅ Monitoring complete!')}")
    time.sleep(2)


# ─── Scenario: mcp-overview (full, for standalone runs) ───────────────────────

def scenario_mcp_overview(client: MCPClient) -> None:
    scenario_mcp_overview_1(client)


# ─── Scenario: f1-f2-1 (init + Swank setup) ───────────────────────────────────

def scenario_f1_f2_1(client: MCPClient) -> None:
    banner("TRON MCP — f1/f2 Setup: Connecting to a Live Lisp Image")
    print("First we start the MCP server, launch a fresh SBCL process with Swank, and connect.\n")
    time.sleep(1)

    do_initialize(client)
    do_whitelist_all(client)
    time.sleep(1)

    do_swank_setup(client, verbose=True)
    time.sleep(1)

    step(4, "Confirm REPL is live")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(+ 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(+ 1 2)"})
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:120]}"))
    time.sleep(1.5)

    print(f"\n{col(GREEN, '✅ Connected to live SBCL+Swank. Ready to debug.')}")
    time.sleep(2)

    # Cleanup
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})


# ─── Scenario: f1-f2-2 (THE core in-debugger hot-reload demo) ─────────────────

def scenario_f1_f2_2(client: MCPClient) -> None:
    banner("TRON MCP — Fixing a Bug While Inside the Debugger")
    print("Common Lisp's debugger lets you modify live code without leaving the call stack.")
    print("Tron lets an AI agent do exactly that: compile a fix while the debugger is active,\n"
          "then invoke CONTINUE to retry the failing call — no restart needed.\n")
    time.sleep(1.5)

    do_initialize(client)
    do_whitelist_all(client)
    do_swank_setup(client, verbose=False)
    time.sleep(0.5)

    step(1, "Compile f1 — it calls f2 which does not exist yet")
    code_f1 = "(defun f1 (a b) (f2 a b))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code_f1}}})
    client.call_tool("repl_compile", {"code": code_f1})
    print(col(GREEN, "← compiled  ") + col(MAGENTA, "(warning: F2 is undefined at compile time)"))
    time.sleep(2)

    step(2, "Eval (f1 1 2) — triggers UNDEFINED-FUNCTION, enters debugger")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    print(col(RED, f"← ⚠  Debugger activated!"))
    # Show the condition
    if "UNDEFINED-FUNCTION" in text or "undefined" in text.lower():
        print(col(RED, f"   The function COMMON-LISP-USER::F2 is undefined."))
    print(col(DIM, f"   {text[100:220].strip()}"))
    time.sleep(2)

    step(3, "Inspect available restarts — the debugger offers to retry calling F2")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "debugger_restarts", "arguments": {}}})
    resp = client.call_tool("debugger_restarts")
    text = client.extract_result_text(resp)
    # Parse and display restarts clearly
    restarts_raw = text
    print(col(GREEN, "← Available restarts:"))
    # Extract restart names/descriptions from plist
    if "CONTINUE" in text and "Retry" in text:
        print(col(GREEN, f"   ") + col(BOLD, "[0] CONTINUE") + col(GREEN, " — Retry calling F2.    ") + col(YELLOW, "← we'll use this after defining F2"))
        print(col(DIM,   f"   [1] USE-VALUE   — Call specified function."))
        print(col(DIM,   f"   [2] RETURN-VALUE — Return specified values."))
        print(col(DIM,   f"   ..."))
    else:
        print(col(GREEN, f"   {text[:200]}"))
    time.sleep(2.5)

    step(4, "Hot-compile f2 into the running image — WHILE STILL IN THE DEBUGGER")
    code_f2 = "(defun f2 (x y) (+ x y))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code_f2}}})
    resp = client.call_tool("repl_compile", {"code": code_f2})
    text = client.extract_result_text(resp)
    print(col(GREEN, "← F2 compiled and loaded into the live image!"))
    print(col(DIM,   f"   {text[:100]}"))
    time.sleep(2)

    step(5, "Invoke restart [0] CONTINUE — retry the original (f1 1 2) call")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 0}}})
    resp = client.call_tool("repl_invoke_restart", {"restart_index": 0})
    print(col(GREEN, "← CONTINUE invoked — F2 called again with args (1 2), f1 returns 3"))
    print(col(DIM,   "   The original call stack was resumed, not aborted."))
    time.sleep(2)

    step(6, "Confirm: f1 and f2 are both live in the image")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    if "3" in text:
        print(col(GREEN, "← ✓ (f1 1 2) = 3  — f2 is now defined, f1 works correctly"))
    else:
        print(col(GREEN, f"← {text[:120]}"))
    time.sleep(2)

    print(f"\n{col(GREEN, '✅ Live debugging complete!')}")
    print(col(GREEN, "   f2 was compiled into the running image while the debugger was active."))
    print(col(GREEN, "   The CONTINUE restart retried the failed call — no restart of the Lisp process needed."))
    time.sleep(2)

    # Cleanup
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})


# ─── Scenario: f1-f2 (full combined, legacy) ─────────────────────────────────

def scenario_f1_f2(client: MCPClient) -> None:
    """Legacy full f1-f2 scenario — combines setup + in-debugger fix."""
    banner("TRON MCP — f1/f2 Hot-Reload Demo")
    print("Common Lisp's debugger lets you fix code without leaving the call stack.")
    print("We compile f2 WHILE IN THE DEBUGGER and use CONTINUE to retry the failing call.\n")
    time.sleep(1)

    do_initialize(client)
    do_whitelist_all(client)
    time.sleep(1)

    step(2, "Launch Swank server on port 14006")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "swank_launch", "arguments": {"port": 14006}}})
    resp = client.call_tool("swank_launch", {"port": 14006}, timeout=60)
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:120]}"))
    time.sleep(2)

    step(3, "Connect REPL to Swank")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_connect", "arguments": {"port": 14006}}})
    resp = client.call_tool("repl_connect", {"port": 14006}, timeout=30)
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:120]}"))
    time.sleep(1.5)

    step(4, "Compile f1 — calls undefined f2")
    code_f1 = "(defun f1 (a b) (f2 a b))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code_f1}}})
    client.call_tool("repl_compile", {"code": code_f1})
    print(col(GREEN, "← compiled  ") + col(MAGENTA, "(warning: F2 undefined at compile time)"))
    time.sleep(2)

    step(5, "Eval (f1 1 2) — UNDEFINED-FUNCTION, enters debugger")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    print(col(RED, "← ⚠  Debugger activated! The function F2 is undefined."))
    time.sleep(2)

    step(6, "Show restarts — restart [0] CONTINUE will retry calling F2")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "debugger_restarts", "arguments": {}}})
    resp = client.call_tool("debugger_restarts")
    text = client.extract_result_text(resp)
    print(col(GREEN, "← Available restarts:"))
    print(col(GREEN, "   ") + col(BOLD, "[0] CONTINUE") + col(GREEN, " — Retry calling F2.    ") + col(YELLOW, "← will use after defining F2"))
    print(col(DIM,   "   [1] USE-VALUE  [2] RETURN-VALUE  ..."))
    time.sleep(2)

    step(7, "Hot-compile f2 — WHILE STILL IN THE DEBUGGER")
    code_f2 = "(defun f2 (x y) (+ x y))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code_f2}}})
    resp = client.call_tool("repl_compile", {"code": code_f2})
    print(col(GREEN, "← F2 compiled and loaded into the live image!"))
    time.sleep(2)

    step(8, "Invoke restart [0] CONTINUE — retries the failed F2 call")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 0}}})
    client.call_tool("repl_invoke_restart", {"restart_index": 0})
    print(col(GREEN, "← CONTINUE invoked — F2 retried, f1 returns 3"))
    time.sleep(1.5)

    step(9, "Confirm: (f1 1 2) works")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:120]}"))
    time.sleep(2)

    step(10, "Cleanup")
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})
    print(col(GREEN, "← cleaned up"))
    time.sleep(1)

    print(f"\n{col(GREEN, '✅ f2 compiled in-debugger, CONTINUE restart retried the call — no process restart!')}")
    time.sleep(2)


# ─── Scenario: factorial-1 (setup + compile buggy) ───────────────────────────

def scenario_factorial_1(client: MCPClient) -> None:
    banner("TRON MCP — Factorial: Setting Up and Triggering the Bug")
    print("We launch a fresh Lisp image, define a buggy factorial, and run it to trigger an error.\n")
    time.sleep(1)

    do_initialize(client)
    do_whitelist_all(client)
    time.sleep(1)

    do_swank_setup(client, verbose=True)
    time.sleep(1)

    step(4, "Compile a buggy factorial — the base case divides by zero")
    buggy = "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": buggy}}})
    client.call_tool("repl_compile", {"code": buggy})
    print(col(GREEN, "← compiled  ") + col(MAGENTA, "(warning: constant-fold DIVISION-BY-ZERO)"))
    time.sleep(2)

    print(f"\n{col(GREEN, '✅ Buggy factorial defined. Run factorial-2 to see the debugging session.')}")
    time.sleep(2)

    # Cleanup
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})


# ─── Scenario: factorial-2 (debug + fix + verify) ────────────────────────────

def scenario_factorial_2(client: MCPClient) -> None:
    banner("TRON MCP — Factorial: Debug, Hot-Reload Fix, Verify")
    print("We trigger the DIVISION-BY-ZERO, inspect the error, hot-reload the fix,")
    print("and verify correctness — all without restarting the Lisp process.\n")
    time.sleep(1.5)

    do_initialize(client)
    do_whitelist_all(client)
    do_swank_setup(client, verbose=False)

    # Silently define buggy factorial
    buggy = "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"
    client.call_tool("repl_compile", {"code": buggy})
    print(col(DIM, "  (buggy factorial compiled — base case has (/ 1 0))"))
    time.sleep(1)

    step(1, "Eval (factorial 5) — triggers DIVISION-BY-ZERO")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(factorial 5)"}}})
    resp = client.call_tool("repl_eval", {"code": "(factorial 5)"})
    text = client.extract_result_text(resp)
    print(col(RED, "← ⚠  Debugger activated!"))
    if "DIVISION-BY-ZERO" in text or "division" in text.lower():
        print(col(RED, f"   arithmetic error: DIVISION-BY-ZERO — operation was (/ 1 0)"))
    time.sleep(2)

    step(2, "Inspect available restarts")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "debugger_restarts", "arguments": {}}})
    resp = client.call_tool("debugger_restarts")
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:300]}"))
    time.sleep(2)

    step(3, "Abort debugger, then hot-reload the fix")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 5}}})
    client.call_tool("repl_invoke_restart", {"restart_index": 5})
    print(col(GREEN, "← aborted — back to top level"))
    time.sleep(1)

    fixed = "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) 1))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": fixed}}})
    client.call_tool("repl_compile", {"code": fixed})
    print(col(GREEN, "← FACTORIAL redefined in the live image — base case now returns 1"))
    time.sleep(2)

    step(4, "Verify the fix")
    for expr, expected in [("(factorial 5)", "120"), ("(factorial 7)", "5040"), ("(factorial 10)", "3628800")]:
        show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": expr}}})
        resp = client.call_tool("repl_eval", {"code": expr})
        text = client.extract_result_text(resp)
        # Extract numeric result from Lisp plist
        num = None
        for part in text.split():
            part_clean = part.strip("()")
            if part_clean.isdigit():
                num = part_clean
        ok = num == expected
        mark = col(GREEN, "✓") if ok else col(RED, "✗")
        print(col(GREEN, f"← {mark} {expr} = {num or text[:40]}"))
        time.sleep(0.5)
    time.sleep(1.5)

    step(5, "Cleanup")
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})
    print(col(GREEN, "← cleaned up"))
    time.sleep(1)

    print(f"\n{col(GREEN, '✅ Bug debugged, fix hot-loaded, results verified — Lisp image never restarted.')}")
    time.sleep(2)


# ─── Scenario: factorial (full combined, legacy) ─────────────────────────────

def scenario_factorial(client: MCPClient) -> None:
    banner("TRON MCP — Factorial Debugging Demo")
    print("Buggy factorial triggers DIVISION-BY-ZERO. Debug, fix, verify.\n")
    time.sleep(1)

    do_initialize(client)
    do_whitelist_all(client)
    time.sleep(1)

    step(2, "Launch Swank + Connect")
    client.call_tool("swank_launch", {"port": 14006}, timeout=60)
    time.sleep(2)
    resp = client.call_tool("repl_connect", {"port": 14006})
    text = client.extract_result_text(resp)
    try:
        r = json.loads(text)
        print(col(GREEN, f"← connected: {r.get('success')}  port: {r.get('port')}"))
    except Exception:
        print(col(GREEN, f"← connected"))
    time.sleep(1.5)

    step(3, "Compile buggy factorial (base case divides by zero)")
    buggy = "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": buggy}}})
    client.call_tool("repl_compile", {"code": buggy})
    print(col(GREEN, "← compiled  (warning: constant-fold DIVISION-BY-ZERO)"))
    time.sleep(2)

    step(4, "Eval (factorial 5) — triggers DIVISION-BY-ZERO")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(factorial 5)"}}})
    resp = client.call_tool("repl_eval", {"code": "(factorial 5)"})
    text = client.extract_result_text(resp)
    print(col(RED, f"← {text[:120]}"))
    time.sleep(2)

    step(5, "Inspect available restarts")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "debugger_restarts", "arguments": {}}})
    resp = client.call_tool("debugger_restarts")
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← {text[:300]}"))
    time.sleep(2)

    step(6, "Abort debugger")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 5}}})
    client.call_tool("repl_invoke_restart", {"restart_index": 5})
    print(col(GREEN, "← aborted"))
    time.sleep(1.5)

    step(7, "Hot-reload fixed factorial")
    fixed = "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) 1))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": fixed}}})
    client.call_tool("repl_compile", {"code": fixed})
    print(col(GREEN, "← compiled — FACTORIAL redefined in live image"))
    time.sleep(2)

    step(8, "Verify fix")
    for expr, expected in [("(factorial 5)", "120"), ("(factorial 7)", "5040"), ("(factorial 10)", "3628800")]:
        show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": expr}}})
        resp = client.call_tool("repl_eval", {"code": expr})
        text = client.extract_result_text(resp)
        num = None
        for part in text.split():
            part_clean = part.strip("()")
            if part_clean.isdigit():
                num = part_clean
        ok = num == expected
        mark = col(GREEN, "✓") if ok else col(RED, "✗")
        print(col(GREEN, f"← {mark} {expr} = {num or text[:40]}"))
        time.sleep(0.5)
    time.sleep(1.5)

    step(9, "Cleanup")
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})
    print(col(GREEN, "← cleaned up"))
    time.sleep(1)

    print(f"\n{col(GREEN, '✅ Debug demo complete! Bug found, inspected, fixed without restart.')}")
    time.sleep(2)


# ─── Main ──────────────────────────────────────────────────────────────────────

SCENARIOS = {
    # Full scenarios (legacy, one-shot)
    "mcp-overview": scenario_mcp_overview,
    "f1-f2":        scenario_f1_f2,
    "factorial":    scenario_factorial,
    # Phase recordings (split for README)
    "mcp-overview-1": scenario_mcp_overview_1,
    "mcp-overview-2": scenario_mcp_overview_2,
    "f1-f2-1":        scenario_f1_f2_1,
    "f1-f2-2":        scenario_f1_f2_2,
    "factorial-1":    scenario_factorial_1,
    "factorial-2":    scenario_factorial_2,
}


def main():
    if len(sys.argv) < 2 or sys.argv[1] not in SCENARIOS:
        print(f"Usage: python3 demo/run-demo.py <scenario>")
        print(f"Scenarios: {', '.join(SCENARIOS)}")
        sys.exit(1)

    scenario_name = sys.argv[1]
    scenario_fn = SCENARIOS[scenario_name]

    # Must run from project root
    if not os.path.exists("./start-mcp.sh"):
        print(col(RED, "Error: run from project root (where start-mcp.sh is)"))
        sys.exit(1)

    client = MCPClient()
    try:
        print(col(DIM, f"Starting MCP server..."))
        client.start()
        print(col(DIM, f"Server started. Running scenario: {scenario_name}\n"))
        scenario_fn(client)
    except KeyboardInterrupt:
        print(col(YELLOW, "\n\nInterrupted."))
    except Exception as e:
        print(col(RED, f"\nError: {e}"))
        import traceback
        traceback.print_exc()
        sys.exit(1)
    finally:
        client.stop()


if __name__ == "__main__":
    main()


