#!/usr/bin/env python3
"""
CL-TRON-MCP Client for Opencode

A Python MCP client that spawns the SBCL cl-tron-mcp server and provides
an easy interface for AI assistants to call debugging/introspection tools.

Usage:
    from cl_tron_client import CLTronClient

    with CLTronClient() as client:
        # Inspect a function
        result = client.inspect_function("CL:CAR")
        print(result)

        # Trace a function
        client.trace_function("MY-PACKAGE::my-function")

        # Evaluate Lisp code
        result = client.repl_eval("(+ 10 20)")
"""

import json
import subprocess
import sys
import time
import os
from typing import Optional, Dict, Any, List
from dataclasses import dataclass
from pathlib import Path


@dataclass
class MCPTool:
    """Represents an MCP tool definition."""

    name: str
    description: str
    input_schema: Dict[str, Any]
    output_schema: Dict[str, Any]
    requires_approval: bool


class CLTronClient:
    """
    MCP client for CL-TRON-MCP (SBCL Debugging Server)

    Spawns the SBCL server and provides high-level Python methods
    for all available debugging and introspection tools.
    """

    def __init__(self, sbcl_path: str = "sbcl", port: Optional[int] = None):
        """
        Initialize the CL-TRON-MCP client.

        Args:
            sbcl_path: Path to SBCL executable (default: "sbcl")
            port: Port for HTTP transport (None for stdio)
        """
        self.sbcl_path = sbcl_path
        self.port = port
        self.process: Optional[subprocess.Popen] = None
        self.tools: Dict[str, MCPTool] = {}
        self._connected = False

    def start(self, timeout: float = 30.0) -> bool:
        """
        Start the CL-TRON-MCP server.

        Args:
            timeout: Seconds to wait for server readiness

        Returns:
            True if server started successfully
        """
        # Build command
        if self.port:
            cmd = [
                self.sbcl_path,
                "--non-interactive",
                "--eval",
                "(ql:quickload :cl-tron-mcp :silent t)",
                "--eval",
                f"(cl-tron-mcp/core:start-server :transport :http :port {self.port})",
            ]
        else:
            cmd = [
                self.sbcl_path,
                "--non-interactive",
                "--eval",
                "(ql:quickload :cl-tron-mcp :silent t)",
                "--eval",
                "(cl-tron-mcp/core:start-server :transport :stdio)",
            ]

        try:
            if self.port:
                # HTTP mode - just start server
                self.process = subprocess.Popen(
                    cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
                )
                # Wait for server to be ready
                time.sleep(2)
                self._connected = True
            else:
                # Stdio mode - use MCP protocol
                self.process = subprocess.Popen(
                    cmd,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    bufsize=0,
                )
                # Wait for ready
                start_time = time.time()
                while time.time() - start_time < timeout:
                    if self._read_message():
                        self._connected = True
                        return True
                    time.sleep(0.1)
                raise TimeoutError("Server did not respond within timeout")

            # Fetch tools list
            self._fetch_tools()
            return True

        except Exception as e:
            print(f"Failed to start CL-TRON-MCP: {e}", file=sys.stderr)
            return False

    def stop(self):
        """Stop the CL-TRON-MCP server."""
        if self.process:
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
            self.process = None
        self._connected = False

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()

    # ==================== INSPECTOR TOOLS ====================

    def inspect_function(self, symbol_name: str) -> Dict[str, Any]:
        """Inspect a function definition."""
        return self._call_tool("inspect_function", {"symbolName": symbol_name})

    def inspect_object(self, object_id: str, max_depth: int = 3) -> Dict[str, Any]:
        """Inspect an object by ID."""
        return self._call_tool(
            "inspect_object", {"objectId": object_id, "maxDepth": max_depth}
        )

    def inspect_class(self, class_name: str) -> Dict[str, Any]:
        """Inspect a CLOS class definition."""
        return self._call_tool("inspect_class", {"className": class_name})

    def inspect_package(self, package_name: str) -> Dict[str, Any]:
        """Inspect a package and list its contents."""
        return self._call_tool("inspect_package", {"packageName": package_name})

    def inspect_slot(
        self, object_id: str, slot_name: str, value: Optional[str] = None
    ) -> Dict[str, Any]:
        """Get or set a slot value on an object."""
        params = {"objectId": object_id, "slotName": slot_name}
        if value:
            params["value"] = value
        return self._call_tool("inspect_slot", params)

    # ==================== DEBUGGER TOOLS ====================

    def get_debugger_frames(
        self, thread: Optional[str] = None, start: int = 0, end: int = 20
    ) -> Dict[str, Any]:
        """Get debugger stack frames."""
        params = {"thread": thread, "start": start, "end": end}
        return self._call_tool("debugger_frames", params)

    def list_restarts(self) -> Dict[str, Any]:
        """List available debugger restarts."""
        return self._call_tool("debugger_restarts", {})

    def set_breakpoint(
        self, function_name: str, condition: Optional[str] = None, hit_count: int = 0
    ) -> Dict[str, Any]:
        """Set a breakpoint on a function."""
        params = {
            "functionName": function_name,
            "condition": condition,
            "hitCount": hit_count,
        }
        return self._call_tool("breakpoint_set", params)

    def remove_breakpoint(self, breakpoint_id: int) -> Dict[str, Any]:
        """Remove a breakpoint by ID."""
        return self._call_tool("breakpoint_remove", {"breakpointId": breakpoint_id})

    def list_breakpoints(self) -> Dict[str, Any]:
        """List all active breakpoints."""
        return self._call_tool("breakpoint_list", {})

    # ==================== REPL TOOLS ====================

    def repl_eval(self, code: str, package: str = "CL-USER") -> Dict[str, Any]:
        """
        Evaluate Lisp code in REPL context.

        Args:
            code: Lisp code to evaluate
            package: Package to use for evaluation

        Returns:
            Dictionary with :result or :error
        """
        return self._call_tool("repl_eval", {"code": code, "package": package})

    # ==================== HOT RELOAD TOOLS ====================

    def compile_and_load(
        self, code: str, filename: str = "repl.lisp"
    ) -> Dict[str, Any]:
        """Compile and load Lisp code."""
        return self._call_tool(
            "code_compile_string", {"code": code, "filename": filename}
        )

    def reload_system(self, system_name: str, force: bool = False) -> Dict[str, Any]:
        """Reload an ASDF system."""
        return self._call_tool(
            "reload_system", {"systemName": system_name, "force": force}
        )

    # ==================== PROFILER TOOLS ====================

    def profile_start(self) -> Dict[str, Any]:
        """Start deterministic profiling."""
        return self._call_tool("profile_start", {})

    def profile_stop(self) -> Dict[str, Any]:
        """Stop profiling."""
        return self._call_tool("profile_stop", {})

    def profile_report(self, format: str = "flat") -> Dict[str, Any]:
        """Get profiling report."""
        return self._call_tool("profile_report", {"format": format})

    # ==================== TRACER TOOLS ====================

    def trace_function(
        self, function_name: str, condition: Optional[str] = None, hit_count: int = 0
    ) -> Dict[str, Any]:
        """Add trace to a function."""
        return self._call_tool(
            "trace_function",
            {
                "functionName": function_name,
                "condition": condition,
                "hitCount": hit_count,
            },
        )

    def trace_remove(self, function_name: str) -> Dict[str, Any]:
        """Remove trace from a function."""
        return self._call_tool("trace_remove", {"functionName": function_name})

    def trace_list(self) -> Dict[str, Any]:
        """List all traced functions."""
        return self._call_tool("trace_list", {})

    # ==================== THREAD TOOLS ====================

    def list_threads(self) -> Dict[str, Any]:
        """List all threads with their status."""
        return self._call_tool("thread_list", {})

    def inspect_thread(self, thread_id: str) -> Dict[str, Any]:
        """Get detailed information about a thread."""
        return self._call_tool("thread_inspect", {"threadId": thread_id})

    def thread_backtrace(self, thread_id: str) -> Dict[str, Any]:
        """Get backtrace for a specific thread."""
        return self._call_tool("thread_backtrace", {"threadId": thread_id})

    # ==================== MONITOR TOOLS ====================

    def health_check(self) -> Dict[str, Any]:
        """Basic health check for the MCP server."""
        return self._call_tool("health_check", {})

    def runtime_stats(self) -> Dict[str, Any]:
        """Get runtime statistics including memory and thread info."""
        return self._call_tool("runtime_stats", {})

    def gc_run(self, generation: int = 0) -> Dict[str, Any]:
        """Force garbage collection."""
        return self._call_tool("gc_run", {"generation": generation})

    def system_info(self) -> Dict[str, Any]:
        """Get comprehensive system information."""
        return self._call_tool("system_info", {})

    # ==================== XREF TOOLS ====================

    def who_calls(self, symbol_name: str) -> Dict[str, Any]:
        """Find functions that call a symbol."""
        return self._call_tool("who_calls", {"symbolName": symbol_name})

    def who_references(self, symbol_name: str) -> Dict[str, Any]:
        """Find references to a symbol."""
        return self._call_tool("who_references", {"symbolName": symbol_name})

    def who_binds(self, symbol_name: str) -> Dict[str, Any]:
        """Find bindings of a symbol."""
        return self._call_tool("who_binds", {"symbolName": symbol_name})

    def who_sets(self, symbol_name: str) -> Dict[str, Any]:
        """Find setq/makunbound of a symbol."""
        return self._call_tool("who_sets", {"symbolName": symbol_name})

    def list_callees(self, symbol_name: str) -> Dict[str, Any]:
        """List functions called by a symbol."""
        return self._call_tool("list_callees", {"symbolName": symbol_name})

    # ==================== LOGGING TOOLS ====================

    def log_configure(
        self, level: str = "info", package: Optional[str] = None
    ) -> Dict[str, Any]:
        """Configure logging level."""
        params = {"level": level}
        if package:
            params["package"] = package
        return self._call_tool("log_configure", params)

    def log_info(self, message: str, package: Optional[str] = None) -> Dict[str, Any]:
        """Log an info message."""
        params = {"message": message}
        if package:
            params["package"] = package
        return self._call_tool("log_info", params)

    def log_debug(self, message: str, package: Optional[str] = None) -> Dict[str, Any]:
        """Log a debug message."""
        params = {"message": message}
        if package:
            params["package"] = package
        return self._call_tool("log_debug", params)

    def log_warn(self, message: str, package: Optional[str] = None) -> Dict[str, Any]:
        """Log a warning message."""
        params = {"message": message}
        if package:
            params["package"] = package
        return self._call_tool("log_warn", params)

    def log_error(self, message: str, package: Optional[str] = None) -> Dict[str, Any]:
        """Log an error message."""
        params = {"message": message}
        if package:
            params["package"] = package
        return self._call_tool("log_error", params)

    # ==================== HELPER METHODS ====================

    def _fetch_tools(self):
        """Fetch the list of available tools from the server."""
        result = self._call_tool("tools/list", {})
        if "tools" in result:
            for tool in result["tools"]:
                self.tools[tool["name"]] = MCPTool(
                    name=tool["name"],
                    description=tool.get("description", ""),
                    input_schema=tool.get("inputSchema", {}),
                    output_schema=tool.get("outputSchema", {}),
                    requires_approval=tool.get("requiresApproval", False),
                )

    def _call_tool(self, tool_name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """
        Call an MCP tool with the given arguments.

        Args:
            tool_name: Name of the tool to call
            arguments: Dictionary of arguments for the tool

        Returns:
            Result dictionary from the server
        """
        request = {
            "jsonrpc": "2.0",
            "id": self._next_id(),
            "method": "tools/call",
            "params": {"name": tool_name, "arguments": arguments},
        }

        if self.port:
            return self._http_request(request)
        else:
            return self._stdio_request(request)

    def _next_id(self) -> int:
        """Generate the next request ID."""
        if not hasattr(self, "_request_id"):
            self._request_id = 0
        self._request_id += 1
        return self._request_id

    def _stdio_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Send a request via stdio and get response."""
        if not self.process:
            raise RuntimeError("Server not started")

        # Send request
        request_line = json.dumps(request)
        self.process.stdin.write(request_line + "\n")  # type: ignore
        self.process.stdin.flush()  # type: ignore

        # Read response
        response = self._read_message()
        if not response:
            raise RuntimeError("No response from server")

        if "error" in response:
            raise RuntimeError(f"MCP Error: {response['error']}")

        return response.get("result", {})

    def _http_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Send a request via HTTP and get response."""
        import urllib.request

        url = f"http://127.0.0.1:{self.port}/rpc"
        data = json.dumps(request).encode("utf-8")

        req = urllib.request.Request(
            url, data=data, headers={"Content-Type": "application/json"}
        )

        with urllib.request.urlopen(req, timeout=30) as response:
            result = json.loads(response.read().decode("utf-8"))

        if "error" in result:
            raise RuntimeError(f"MCP Error: {result['error']}")

        return result.get("result", {})

    def _read_message(self) -> Optional[Dict[str, Any]]:
        """Read a JSON-RPC message from stdio."""
        if not self.process:
            return None

        line = self.process.stdout.readline()  # type: ignore
        if not line:
            return None

        line = line.strip()
        if not line:
            return None

        try:
            return json.loads(line)
        except json.JSONDecodeError:
            return None


def demo():
    """Run a quick demo of the CL-TRON-MCP client."""
    print("=" * 60)
    print("CL-TRON-MCP Client Demo")
    print("=" * 60)

    # Check if SBCL is available
    import shutil

    if not shutil.which("sbcl"):
        print("Error: SBCL not found in PATH")
        print("Install SBCL: https://www.sbcl.org/getting.html")
        return

    with CLTronClient() as client:
        print(f"\nConnected! {len(client.tools)} tools available.\n")

        # Health check
        print("[1] Health Check:")
        health = client.health_check()
        print(f"   Status: {health.get('status', 'unknown')}")

        # Inspect a function
        print("\n[2] Inspect CL:CAR:")
        result = client.inspect_function("CL:CAR")
        print(f"   Symbol: {result.get('symbol', 'N/A')}")
        print(f"   Type: {result.get('type', 'N/A')}")

        # List threads
        print("\n[3] List Threads:")
        threads = client.list_threads()
        print(f"   {threads}")

        # System info
        print("\n[4] System Info:")
        info = client.system_info()
        print(f"   Lisp: {info.get('lisp-implementation', 'N/A')}")
        print(f"   Packages: {info.get('packages-count', 'N/A')}")

        # Evaluate some Lisp
        print("\n[5] REPL Eval (+ 10 20):")
        result = client.repl_eval("(+ 10 20)")
        print(f"   Result: {result.get('result', 'N/A')}")

        print("\n" + "=" * 60)
        print("Demo complete!")
        print("=" * 60)


if __name__ == "__main__":
    demo()
