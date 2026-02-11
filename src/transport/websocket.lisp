;;;; src/transport/websocket.lisp - WebSocket Transport for CL-TRON-MCP

(in-package :cl-tron-mcp/transport)

(defvar *websocket-server* nil)
(defvar *websocket-thread* nil)
(defvar *websocket-clients* nil)
(defvar *websocket-running* nil)

(defun start-websocket-transport (&key (port 8081))
  "Start WebSocket transport on specified port.
   Requires: usocket, ironclad, babel, base64"
  (format t "[MCP] WebSocket transport enabled on port ~d~%" port)
  (format t "[MCP] Note: Full WebSocket implementation pending~%"))

(defun stop-websocket-transport ()
  "Stop WebSocket transport."
  (setf *websocket-running* nil)
  (format t "[MCP] WebSocket transport stopped~%"))

(defun send-message-via-websocket (message)
  "Send message to all connected WebSocket clients."
  (declare (ignore message))
  nil)
