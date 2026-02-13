;;;; src/protocol/handlers.lisp

(in-package :cl-tron-mcp/protocol)

(defvar *message-handler* nil)
(defvar *request-id* nil)

(defun handle-message (message)
  "Handle incoming JSON-RPC message."
  (let* ((parsed (if (stringp message) (jonathan:parse message) message))
         (id (getf parsed :|id|))
         (method (getf parsed :|method|))
         (params (getf parsed :|params|)))
    (handler-case
        (cond
          ((null id)
           (handle-notification method params))
          (t
           (handle-request id method params)))
      (error (e)
        (make-error-response id -32000 (princ-to-string e))))))

(defun handle-request (id method params)
  "Handle JSON-RPC 2.0 request."
  (let ((*request-id* id))
    (cond
      ((string= method "initialize")
       (handle-initialize id params))
      ((string= method "tools/list")
       (handle-tools-list id))
      ((string= method "tools/call")
       (handle-tool-call id params))
      ((string= method "ping")
       (handle-ping id))
      (t
       (make-error-response id -32601 (format nil "Unknown method: ~a" method))))))

(defun handle-notification (method params)
  "Handle JSON-RPC 2.0 notification. Logged via log4cl (not stdout) so stdio transport stays clean."
  (cl-tron-mcp/logging:log-info (format nil "Notification: ~a ~a" method params))
  nil)

(defun handle-initialize (id params)
  "Handle initialize request."
  (jonathan:to-json (list :|jsonrpc| "2.0"
                          :|id| 1
                          :|result| (list :|protocolVersion| "2024-11-05"
                                       :|capabilities| (list)
                                       :|serverInfo| (list :|name| "cl-tron-mcp"
                                                        :|version| "0.1.0")))))

(defun handle-tools-list (id)
  "Handle tools/list request."
  (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
    (jonathan:to-json (list :|jsonrpc| "2.0"
                            :|id| id
                            :|result| (list :|tools| tools)))))

(defun handle-tool-call (id params)
  "Handle tools/call request."
  (let ((tool-name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (handler-case
        (let ((result (cl-tron-mcp/tools:call-tool tool-name arguments)))
          (jonathan:to-json (list :|jsonrpc| "2.0"
                                  :|id| id
                                  :|result| (list :|content| (list :|type| "text"
                                                           :|text| (format nil "~a" result))))))
      (error (e)
        (make-error-response id -32000 (princ-to-string e))))))

(defun handle-ping (id)
  "Handle ping request."
  (jonathan:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|result| (list :|pong| t))))

(defun make-error-response (id code message)
  "Create JSON-RPC error response."
  (jonathan:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|error| (list :|code| code
                                       :|message| message))))
