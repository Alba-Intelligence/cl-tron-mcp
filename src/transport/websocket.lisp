;;;; src/transport/websocket.lisp - WebSocket Transport for CL-TRON-MCP
;;;;
;;;; WebSocket transport is not yet implemented.
;;;; Use :stdio-only or :http-only instead.

(in-package :cl-tron-mcp/transport)

(defvar *websocket-server* nil)
(defvar *websocket-thread* nil)
(defvar *websocket-clients* nil)
(defvar *websocket-running* nil)

(defun start-websocket-transport (&key (port 4006))
  "WebSocket transport is not yet implemented.
Use :stdio-only or :http-only transport instead.

Example:
  (cl-tron-mcp/core:start-server :transport :http-only :port 4006)
  (cl-tron-mcp/core:start-server :transport :stdio-only)"
  (declare (ignore port))
  (cl-tron-mcp/logging:log-error
   "[MCP] WebSocket transport is not yet implemented. Use :stdio-only or :http-only.")
  (error "WebSocket transport not implemented. Use :stdio-only or :http-only transport."))

(defun stop-websocket-transport ()
  "Stop WebSocket transport (no-op; not implemented)."
  (setf *websocket-running* nil))

(defun send-message-via-websocket (message)
  "Send message to all connected WebSocket clients (not implemented)."
  (declare (ignore message))
  nil)
