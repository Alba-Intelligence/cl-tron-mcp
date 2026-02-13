;;;; src/transport/stdio.lisp

(in-package :cl-tron-mcp/transport)

(defvar *transport* nil)
(defvar *transport-type* :stdio)
(defvar *running* nil)

(defun start-stdio-transport (&key (handler #'cl-tron-mcp/protocol:handle-message)
                                (output #'cl-tron-mcp/transport::send-message-via-stdio))
  "Start stdio transport."
  (format t "[MCP] Starting stdio transport (MCP protocol)~%")
  (force-output)
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
                 (format *error-output* "Error: ~a~%" e)
                 (force-output *error-output*))))))

(defun stop-stdio-transport ()
  "Stop stdio transport."
  (setq *running* nil))

(defun send-message-via-stdio (message)
  "Send message via stdio."
  (format *standard-output* "~a~%" (jonathan:to-json message))
  (force-output *standard-output*))
