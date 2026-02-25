#!/usr/bin/env python3
"""Test client for CL-TRON-MCP Docker container."""

import sys
import urllib.request
import json
import time

MCP_HOST = sys.argv[1] if len(sys.argv) > 1 else "localhost"
MCP_PORT = sys.argv[2] if len(sys.argv) > 2 else ""
BASE_URL = f"http://{MCP_HOST}:{MCP_PORT}"


def http_request(method, params=None):
    """Send HTTP request to MCP server."""
    data = {"jsonrpc": "2.0", "id": 1, "method": method, "params": params or {}}
    req = urllib.request.Request(
        f"{BASE_URL}/rpc",
        data=json.dumps(data).encode("utf-8"),
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(req, timeout=30) as response:
        return json.loads(response.read().decode("utf-8"))


def main():
    print("=" * 60)
    print("CL-TRON-MCP Docker Test Client")
    print("=" * 60)
    print(f"Connecting to: {BASE_URL}")
    print()

    # Wait for server to be ready
    print("[1] Waiting for server...")
    for i in range(10):
        try:
            result = http_request("tools/list")
            if "tools" in result:
                print(f"    Server ready! {len(result['tools'])} tools available.")
                break
        except Exception as e:
            if i == 9:
                print(f"    Error: {e}")
                return
            time.sleep(1)

    # Health check
    print("\n[2] Health Check:")
    try:
        result = http_request("tools/call", {"name": "health_check", "arguments": {}})
        print(f"    Status: {result}")
    except Exception as e:
        print(f"    Error: {e}")

    # System info
    print("\n[3] System Info:")
    try:
        result = http_request("tools/call", {"name": "system_info", "arguments": {}})
        info = result.get("result", {})
        print(f"    Lisp: {info.get('lisp-implementation', 'N/A')}")
        print(f"    Packages: {info.get('packages-count', 'N/A')}")
    except Exception as e:
        print(f"    Error: {e}")

    # Inspect function
    print("\n[4] Inspect CL:CAR:")
    try:
        result = http_request(
            "tools/call",
            {"name": "inspect_function", "arguments": {"symbolName": "CL:CAR"}},
        )
        print(f"    {result.get('result', 'N/A')}")
    except Exception as e:
        print(f"    Error: {e}")

    # REPL eval
    print("\n[5] Evaluate (+ 10 20):")
    try:
        result = http_request(
            "tools/call",
            {
                "name": "repl_eval",
                "arguments": {"code": "(+ 10 20)", "package": "CL-USER"},
            },
        )
        print(f"    Result: {result}")
    except Exception as e:
        print(f"    Error: {e}")

    print("\n" + "=" * 60)
    print("Test complete!")
    print("=" * 60)


if __name__ == "__main__":
    main()
