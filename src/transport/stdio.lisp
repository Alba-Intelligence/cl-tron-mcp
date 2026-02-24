;;;; src/transport/stdio.lisp
;;;; Stdio transport for MCP. CRITICAL: stdout must contain only newline-delimited
;;;; JSON-RPC messages; all other output (logs, errors) goes via log4cl (configured
;;;; to stderr for stdio by ensure-log-to-stream in server.lisp).

(in-package :cl-tron-mcp/transport)

(defvar *transport* nil)
(defvar *transport-type* :stdio)
(defvar *running* nil)

(defun start-stdio-transport (&key (handler #'cl-tron-mcp/protocol:handle-message)
                                (output #'cl-tron-mcp/transport::send-message-via-stdio))
  "Start stdio transport. Activity is logged via log4cl (stderr); only JSON responses go to stdout."
  (cl-tron-mcp/logging:log-info "[MCP] Starting stdio transport (MCP protocol)")
  (setq *running* t)
  (loop while *running*
        do (let ((line (read-line *standard-input* nil)))
             (when (null line) (return))
             (handler-case
                 (let* ((message (jonathan:parse line))
                        (response (funcall handler message)))
                   (when response
                     (funcall output response)))
               (error (e)
                 (cl-tron-mcp/logging:log-error (format nil "Error: ~a" e)))))))

(defun stop-stdio-transport ()
  "Stop stdio transport."
  (setq *running* nil))

(defun send-message-via-stdio (message)
  "Send a single JSON-RPC message to stdout. CRITICAL: This is the only place that must write to stdout for stdio transport.
   Handlers return already-serialized JSON strings; write as-is to avoid double-encoding (Cursor expects object, not string)."
  (format *standard-output* "~a~%"
          (if (stringp message) message (jonathan:to-json message)))
  (force-output *standard-output*))
