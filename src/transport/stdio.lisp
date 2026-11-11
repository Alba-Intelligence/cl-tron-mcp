;;;; src/transport/stdio.lisp
;;;; Stdio transport for MCP. CRITICAL: stdout must contain only newline-delimited
;;;; JSON-RPC messages; all other output (logs, errors) goes via log4cl (configured
;;;; to stderr for stdio by ensure-log-to-stream in server.lisp).

(in-package :cl-tron-mcp/transport)

(defvar *transport* nil)
(defvar *transport-type* :stdio)
(defvar *running* nil)

(defun json-rpc-error-string (id code message)
  "Serialized JSON-RPC error response. ID nil is sent as JSON null."
  (cl-tron-mcp/json-compat:to-json
   (list :|jsonrpc| "2.0" :|id| (or id :null)
         :|error| (list :|code| code :|message| message))))

(defun process-stdio-line (line handler output)
  "Handle one input LINE: parse it, call HANDLER with the message, pass any response to OUTPUT.
Every request gets exactly one reply, even on failure: unparseable input yields -32700 and a
handler error yields -32603 carrying the request id, so a client waiting on that id never hangs.
Blank lines and notifications (no id) produce no output on error."
  (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
    (when (plusp (length text))
      (let ((message (handler-case (cl-tron-mcp/json-compat:parse text)
                       (error (e)
                         (cl-tron-mcp/logging:log-error (format nil "Parse error on stdin: ~a" e))
                         (funcall output (json-rpc-error-string nil -32700 "Parse error"))
                         (return-from process-stdio-line nil)))))
        (let ((id (and (listp message) (ignore-errors (getf message :|id|)))))
          (handler-case
              (let ((response (funcall handler message)))
                (when response
                  (funcall output response)))
            (error (e)
              (cl-tron-mcp/logging:log-error (format nil "Error handling message: ~a" e))
              (when id
                (funcall output (json-rpc-error-string id -32603 (princ-to-string e)))))))))))

(defun start-stdio-transport (&key (handler #'cl-tron-mcp/protocol:handle-message)
                                (output #'cl-tron-mcp/transport::send-message-via-stdio))
  "Start stdio transport. Activity is logged via log4cl (stderr); only JSON responses go to stdout."
  (cl-tron-mcp/logging:log-info "[MCP] Starting stdio transport (MCP protocol)")
  (setq *running* t)
  (loop while *running*
        do (let ((line (read-line *standard-input* nil)))
             (when (null line) (return))
             (process-stdio-line line handler output))))

(defun stop-stdio-transport ()
  "Stop stdio transport."
  (setq *running* nil))

(defun send-message-via-stdio (message)
  "Send a single JSON-RPC message to stdout. CRITICAL: This is the only place that must write to stdout for stdio transport.
   Handlers return already-serialized JSON strings; write as-is to avoid double-encoding (Cursor expects object, not string)."
  (format *standard-output* "~a~%"
          (if (stringp message) message (cl-tron-mcp/json-compat:to-json message)))
  (force-output *standard-output*))
