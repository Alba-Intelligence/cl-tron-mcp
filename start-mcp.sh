#!/bin/bash
# CL-TRON-MCP Server Startup Script

# Navigate to the cl-tron-mcp directory
cd "$(dirname "$0")"

# Start SBCL with the MCP server
exec sbcl --non-interactive \
	--eval '(ql:quickload :cl-tron-mcp :silent t)' \
	--eval '(cl-tron-mcp/core:start-server :transport :stdio)'
