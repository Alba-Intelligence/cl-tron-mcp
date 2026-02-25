#!/bin/bash
cd "$(dirname "$0")/.."
sbcl --non-interactive \
	--eval '(push #p"'"$(pwd)"'/" ql:*local-project-directories*)' \
	--eval '(asdf:load-system :cl-tron-mcp)' \
	--eval '(cl-tron-mcp/transport:http-server-loop 9010)'
