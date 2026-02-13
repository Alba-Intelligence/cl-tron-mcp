;;;; src/core/server.lisp
;;;; MCP server entry. CRITICAL for stdio: all [MCP] activity is logged via log4cl;
;;;; for :stdio we call ensure-log-to-stream(*error-output*) so log4cl writes to
;;;; stderr and stdout stays clean for JSON-RPC only.

(in-package :cl-tron-mcp/core)

(defvar *server-state* :stopped)
(defvar *current-transport* nil)
(defvar *transport-thread* nil)

(defun start-server (&key (transport :stdio) (port 8080))
  "Start the MCP server with the specified transport.
   TRANSPORT can be :stdio, :http, or :websocket.
   PORT is used for HTTP/WebSocket transports.
   For :stdio, log4cl is configured to stderr so stdout contains only JSON-RPC."
  (when (eq *server-state* :running)
    (cl-tron-mcp/logging:log-warn "[MCP] Server is already running")
    (return-from start-server))
  (setq *server-state* :running)
  (when (eq transport :stdio)
    (cl-tron-mcp/logging:ensure-log-to-stream *error-output*))
  (cl-tron-mcp/logging:log-info (format nil "[MCP] Starting server with ~a transport" transport))
  (case transport
    (:stdio
     (start-stdio-transport))
    (:http
     (start-http-transport port))
    (:websocket
     (start-websocket-transport port))
    (t
     (cl-tron-mcp/logging:log-error (format nil "[MCP] Unknown transport: ~a" transport))
     (setq *server-state* :stopped)))
  *server-state*)

(defun stop-server ()
  "Stop the MCP server."
  (when (eq *server-state* :stopped)
    (cl-tron-mcp/logging:log-warn "[MCP] Server is not running")
    (return-from stop-server))
  (cl-tron-mcp/logging:log-info "[MCP] Stopping server...")
  (case *current-transport*
    (:stdio (cl-tron-mcp/transport:stop-stdio-transport))
    (:http (cl-tron-mcp/transport:stop-http-transport))
    (:websocket (cl-tron-mcp/transport:stop-websocket-transport)))
  (setq *current-transport* nil)
  (setq *server-state* :stopped)
  (cl-tron-mcp/logging:log-info "[MCP] Server stopped"))

(defun start-stdio-transport ()
  "Start stdio transport."
  (setq *current-transport* :stdio)
  (cl-tron-mcp/transport:start-stdio-transport))

(defun start-http-transport (port)
  "Start HTTP transport."
  (setq *current-transport* :http)
  (cl-tron-mcp/transport:start-http-transport :port port))

(defun start-websocket-transport (port)
  "Start WebSocket transport."
  (setq *current-transport* :websocket)
  (cl-tron-mcp/transport:start-websocket-transport :port port))

(defun get-server-state ()
  "Get current server state."
  *server-state*)

(defun get-transport-type ()
  "Get current transport type."
  *current-transport*)
