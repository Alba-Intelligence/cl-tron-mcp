;;;; src/transport/http.lisp

(in-package :cl-tron-mcp/transport)

(defun start-transport (&key (port 8080) (handler #'cl-tron-mcp/protocol:handle-message))
  "Start HTTP transport - requires Hunchentoot."
  (declare (ignore port handler))
  (format *error-output* "[MCP] HTTP transport requires Hunchentoot.~%")
  (format *error-output* "[MCP] Add :hunchentoot to cl-tron-mcp.asd dependencies and quickload it.~%"))

(defun stop-transport ()
  "Stop HTTP transport."
  nil)
