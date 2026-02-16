#!/usr/bin/env bash
cd $HOME/quicklisp/local-projects/cl-tron-mcp
exec sbcl --non-interactive --noinform \
  --eval '(setq *compile-verbose* nil *load-verbose* nil)' \
  --eval '(ql:quickload :cl-tron-mcp :silent t)' \
  --eval '(cl-tron-mcp/core:start-server :transport :stdio)'
