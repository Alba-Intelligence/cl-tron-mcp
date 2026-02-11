;;;; src/transport/stdio.lisp

(in-package :cl-tron-mcp/transport)

(defvar *transport* nil)
(defvar *transport-type* :stdio)
(defvar *running* nil)

(defun start-transport (&key (handler #'cl-tron-mcp/protocol:handle-message))
  "Start stdio transport."
  (setq *running* t)
  (loop while *running*
        do (let ((line (read-line *standard-input* nil)))
             (when (null line) (return))
             (handler-case
                 (let ((message (jonathan:parse line)))
                   (funcall handler message))
               (error (e)
                 (format *error-output* "Error: ~a~%" e))))))

(defun stop-transport ()
  "Stop stdio transport."
  (setq *running* nil))

(defun send-message (message)
  "Send message via stdio."
  (format *standard-output* "~a~%" (jonathan:to-json message))
  (force-output *standard-output*))
