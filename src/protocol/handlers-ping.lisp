;;;; src/protocol/handlers-ping.lisp
;;;;
;;;; Ping handler for JSON-RPC protocol.
;;;;
;;;; This file contains:
;;;;   - ping handler (keepalive)

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Ping Handler
;;; ============================================================

(defun handle-ping (id)
  "Handle ping request.
Returns pong response for keepalive."
  (jonathan:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|result| (list :|pong| t))))

(provide :cl-tron-mcp/protocol-handlers-ping)
