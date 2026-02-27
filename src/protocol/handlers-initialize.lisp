;;;; src/protocol/handlers-initialize.lisp
;;;;
;;;; Initialize handler for JSON-RPC protocol.
;;;;
;;;; This file contains:
;;;;   - initialize handler (server handshake and capability negotiation)

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Initialize Handler
;;; ============================================================

(defun handle-initialize (id params)
  "Handle initialize request.
Returns server capabilities including tools, resources, and prompts.
This is the first message sent by MCP clients during handshake.
Response is used by both stdio and HTTP transports unchanged."
  (declare (ignore params))
  (jonathan:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|result| (list :|protocolVersion| "2024-11-05"
                                          :|capabilities| (list
                                                           ;; Tools capability - allows model-controlled tool invocation
                                                           :|tools| (list :|listChanged| t)
                                                           ;; Resources capability - exposes documentation
                                                           :|resources| (list :|subscribe| :false
                                                                             :|listChanged| t)
                                                           ;; Prompts capability - exposes guided workflows
                                                           :|prompts| (list :|listChanged| t))
                                          :|serverInfo| (list :|name| "cl-tron-mcp"
                                                              :|version| cl-tron-mcp/core:*version*)))))

(provide :cl-tron-mcp/protocol-handlers-initialize)
