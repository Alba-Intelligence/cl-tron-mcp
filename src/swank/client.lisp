;;;; src/swank/client.lisp — DEPRECATED
;;;;
;;;; The Swank client has been split into focused modules:
;;;;
;;;;   swank-connection.lisp  — connection state, connect/disconnect, protocol I/O
;;;;   swank-rpc.lisp         — request-response correlation, reader loop, dispatch
;;;;   swank-events.lisp      — event queue, reconnection, event processor
;;;;   swank-api.lisp         — high-level RPC operations, MCP wrappers
;;;;
;;;; This file is kept for backward compatibility but contains no code.
;;;; See cl-tron-mcp.asd for the updated load order.

(in-package #:cl-tron-mcp/swank)
