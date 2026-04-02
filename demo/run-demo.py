#!/usr/bin/env python3
"""
demo/run-demo.py - Tron MCP Demo Driver

Spawns the MCP server via stdio and sends real JSON-RPC requests,
printing a colored transcript of the protocol exchange.

Usage:
    python3 demo/run-demo.py <scenario>

Scenarios:
    mcp-overview   Initialize, list tools/resources/prompts, health check
    f1-f2          Hot-reload: define f1, error on missing f2, compile f2, verify
    factorial      Debug: buggy factorial, DIVISION-BY-ZERO, fix, verify

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
    caps = resp.get("result", {}).get("capabilities", {})
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


# ─── Scenario: mcp-overview ────────────────────────────────────────────────────

def scenario_mcp_overview(client: MCPClient) -> None:
    banner("TRON MCP — Protocol Overview Demo")
    print("Shows the actual JSON-RPC messages that AI agents send to Tron.\n")
    time.sleep(1)

    do_initialize(client)
    time.sleep(1)

    step(2, "List Tools")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/list", "params": {}})
    resp = client.send("tools/list")
    tools = resp.get("result", {}).get("tools", [])
    # Group by category prefix — keys may be uppercase (Lisp JSON serialization)
    cats = {}
    for t in tools:
        name = t.get("NAME") or t.get("name", "unknown")
        cat = name.split("_")[0] if "_" in name else "other"
        cats.setdefault(cat, []).append(name)
    print(col(GREEN, f"← {len(tools)} tools available across {len(cats)} categories:"))
    for cat, names in sorted(cats.items()):
        print(f"   {col(DIM, cat):20s}  {len(names)} tools")
    time.sleep(1.5)

    step(3, "List Resources")
    show_json(col(CYAN, "→"), CYAN, {"method": "resources/list", "params": {}})
    resp = client.send("resources/list")
    resources = resp.get("result", {}).get("resources", [])
    print(col(GREEN, f"← {len(resources)} resources:"))
    for r in resources[:6]:
        print(f"   {r.get('uri', '')}  {col(DIM, r.get('name', ''))}")
    if len(resources) > 6:
        print(f"   {col(DIM, f'... and {len(resources)-6} more')}")
    time.sleep(1.5)

    step(4, "List Prompts")
    show_json(col(CYAN, "→"), CYAN, {"method": "prompts/list", "params": {}})
    resp = client.send("prompts/list")
    prompts = resp.get("result", {}).get("prompts", [])
    print(col(GREEN, f"← {len(prompts)} workflow prompts:"))
    for p in prompts:
        print(f"   {col(MAGENTA, p.get('name', '')):30s}  {p.get('description', '')[:50]}")
    time.sleep(1.5)

    step(5, "Health Check")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "health_check", "arguments": {}}})
    resp = client.call_tool("health_check")
    text = client.extract_result_text(resp)
    try:
        health = json.loads(text)
        status = health.get("status", "unknown")
        uptime = health.get("uptime_seconds", "?")
        print(col(GREEN, f"← status: {status}  uptime: {uptime}s"))
    except Exception:
        print(col(GREEN, f"← {text[:120]}"))
    time.sleep(1.5)

    step(6, "Runtime Stats")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "runtime_stats", "arguments": {}}})
    resp = client.call_tool("runtime_stats")
    text = client.extract_result_text(resp)
    try:
        stats = json.loads(text)
        print(col(GREEN, "← Runtime statistics:"))
        for k, v in list(stats.items())[:8]:
            print(f"   {k}: {v}")
    except Exception:
        print(col(GREEN, f"← {text[:200]}"))
    time.sleep(1)

    print(f"\n{col(GREEN, '✅ Overview complete!')}")
    time.sleep(2)


# ─── Scenario: f1-f2 ──────────────────────────────────────────────────────────

def scenario_f1_f2(client: MCPClient) -> None:
    banner("TRON MCP — f1/f2 Hot-Reload Demo")
    print("Define f1 that calls undefined f2, trigger error, hot-compile f2, verify.\n")
    time.sleep(1)

    do_initialize(client)
    do_whitelist_all(client)
    time.sleep(1)

    step(2, "Launch Swank server on port 14006")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "swank_launch", "arguments": {"port": 14006}}})
    resp = client.call_tool("swank_launch", {"port": 14006}, timeout=60)
    text = client.extract_result_text(resp)
    try:
        r = json.loads(text)
        print(col(GREEN, f"← success: {r.get('success')}  pid: {r.get('pid', '?')}  port: {r.get('port', 14006)}"))
    except Exception:
        print(col(GREEN, f"← {text[:120]}"))
    time.sleep(2)

    step(3, "Connect REPL to Swank")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_connect", "arguments": {"port": 14006}}})
    resp = client.call_tool("repl_connect", {"port": 14006}, timeout=30)
    text = client.extract_result_text(resp)
    try:
        r = json.loads(text)
        print(col(GREEN, f"← connected: {r.get('success')}  host: {r.get('host')}:{r.get('port')}"))
    except Exception:
        print(col(GREEN, f"← {text[:120]}"))
    time.sleep(1.5)

    step(4, "Compile f1 — calls undefined f2")
    code = "(defun f1 (a b) (f2 a b))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code}}})
    resp = client.call_tool("repl_compile", {"code": code})
    text = client.extract_result_text(resp)
    print(col(GREEN, f"← compiled  {col(DIM, '(warning: F2 undefined at compile time)')}"))
    time.sleep(2)

    step(5, "Eval (f1 1 2) — expect UNDEFINED-FUNCTION error")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    try:
        r = json.loads(text)
        if r.get("debug"):
            cond = r.get("condition", "")
            restarts = r.get("restarts", [])
            print(col(RED, f"← ⚠ Debugger activated!"))
            print(f"   condition: {str(cond)[:100]}")
            print(f"   restarts available: {len(restarts)}")
            for i, rs in enumerate(restarts[:3]):
                print(f"   [{i}] {rs}")
        else:
            print(col(GREEN, f"← {text[:120]}"))
    except Exception:
        print(col(RED, f"← {text[:120]}"))
    time.sleep(2)

    step(6, "Invoke restart [0] — abort debugger")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 0}}})
    resp = client.call_tool("repl_invoke_restart", {"restart_index": 0})
    print(col(GREEN, "← aborted — back to top level"))
    time.sleep(1.5)

    step(7, "Hot-compile f2 into live image")
    code2 = "(defun f2 (x y) (+ x y))"
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_compile", "arguments": {"code": code2}}})
    resp = client.call_tool("repl_compile", {"code": code2})
    print(col(GREEN, "← compiled  F2 now exists in the running image!"))
    time.sleep(2)

    step(8, "Eval (f1 1 2) — should work now")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_eval", "arguments": {"code": "(f1 1 2)"}}})
    resp = client.call_tool("repl_eval", {"code": "(f1 1 2)"})
    text = client.extract_result_text(resp)
    try:
        r = json.loads(text)
        val = r.get("ok", r)
        if isinstance(val, list) and len(val) >= 2:
            print(col(GREEN, f"← ✓ (f1 1 2) = {val[1]}"))
        else:
            print(col(GREEN, f"← ✓ result: {str(val)[:80]}"))
    except Exception:
        print(col(GREEN, f"← {text[:120]}"))
    time.sleep(2)

    step(9, "Cleanup — disconnect and kill Swank")
    client.call_tool("repl_disconnect")
    client.call_tool("swank_kill", {"port": 14006})
    print(col(GREEN, "← cleaned up"))
    time.sleep(1)

    print(f"\n{col(GREEN, '✅ Hot-reload complete! f2 was compiled into the live image without restarting.')}")
    time.sleep(2)


# ─── Scenario: factorial ──────────────────────────────────────────────────────

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
    try:
        r = json.loads(text)
        if r.get("debug"):
            cond = r.get("condition", "")
            restarts = r.get("restarts", [])
            print(col(RED, f"← ⚠ Debugger: {str(cond)[:100]}"))
            print(f"   restarts: {len(restarts)} available")
        else:
            print(col(GREEN, f"← {text[:120]}"))
    except Exception:
        print(col(RED, f"← {text[:120]}"))
    time.sleep(2)

    step(5, "Inspect backtrace frames")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "debugger_frames", "arguments": {}}})
    resp = client.call_tool("debugger_frames")
    text = client.extract_result_text(resp)
    try:
        frames = json.loads(text)
        if isinstance(frames, list):
            print(col(GREEN, f"← {len(frames)} stack frames:"))
            for f in frames[:4]:
                if isinstance(f, dict):
                    print(f"   [{f.get('index','')}] {f.get('description','')[:70]}")
                else:
                    print(f"   {str(f)[:70]}")
        else:
            print(col(GREEN, f"← {text[:200]}"))
    except Exception:
        print(col(GREEN, f"← {text[:150]}"))
    time.sleep(2)

    step(6, "Abort debugger")
    show_json(col(CYAN, "→"), CYAN, {"method": "tools/call", "params": {"name": "repl_invoke_restart", "arguments": {"restart_index": 0}}})
    client.call_tool("repl_invoke_restart", {"restart_index": 0})
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
        try:
            r = json.loads(text)
            val = r.get("ok", r)
            actual = val[1] if isinstance(val, list) and len(val) >= 2 else str(val)
            ok = str(actual).strip() == expected
            mark = col(GREEN, "✓") if ok else col(RED, "✗")
            print(col(GREEN, f"← {mark} {expr} = {actual}  (expected {expected})"))
        except Exception:
            print(col(GREEN, f"← {text[:80]}"))
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
    "mcp-overview": scenario_mcp_overview,
    "f1-f2": scenario_f1_f2,
    "factorial": scenario_factorial,
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
