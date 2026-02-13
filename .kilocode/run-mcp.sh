#!/bin/bash
cd /home/emmanuel/quicklisp/local-projects/cl-tron-mcp
exec sbcl --non-interactive --eval '(ql:quickload :cl-tron-mcp :silent t)' --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
