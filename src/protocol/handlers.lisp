;;;; src/protocol/handlers.lisp
;;;;
;;;; Main JSON-RPC 2.0 request handlers for MCP protocol.
;;;;
;;;; This file is the entry point for all MCP protocol handlers.
;;;; It provides message dispatch and loads specialized handler modules.
;;;;
;;;; Handler modules:
;;;;   - handlers-utils.lisp    : Validation, error recovery, utilities
;;;;   - handlers-initialize.lisp: Initialize handler
;;;;   - handlers-tools.lisp    : Tools handlers (list, call, approval)
;;;;   - handlers-resources.lisp: Resources handlers (list, read)
;;;;   - handlers-prompts.lisp  : Prompts handlers (list, get)
;;;;   - handlers-ping.lisp     : Ping handler
;;;;
;;;; See: https://modelcontextprotocol.io/docs/concepts/tools
;;;; See: https://modelcontextprotocol.io/docs/concepts/resources
;;;; See: https://modelcontextprotocol.io/docs/concepts/prompts

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Global State
;;; ============================================================

(defvar *message-handler* nil)
(defvar *request-id* nil)

(defvar *default-tool-timeout* 30 "Default timeout for tool execution in seconds.")

(defvar *pending-requests* (make-hash-table :test 'eql)
  "Hash table tracking pending requests for cleanup.")

(defvar *request-lock* (bordeaux-threads:make-lock "pending-requests")
  "Lock for synchronizing access to *pending-requests*.")

;;; ============================================================
;;; Message Dispatch
;;; ============================================================

(defun handle-message (message)
  "Handle incoming JSON-RPC message.
Dispatches to appropriate handler based on method name.
Messages without ID are treated as notifications."
  (handler-case (let* ((parsed (if (stringp message)
                                   (jonathan:parse message)
                                 message))
                       (id (getf parsed :|id|))
                       (method (getf parsed :|method|))
                       (params (getf parsed :|params|)))
                  (cond
                   ((null id)
                    (handle-notification method params))
                   (t (handle-request id method params))))
    (jonathan.error:<jonathan-error> (e)
                                     (cl-tron-mcp/logging:log-error (format nil "JSON parse error: ~a" e))
                                     (make-error-response nil -32700 "Parse error"))
    (error (e)
           (cl-tron-mcp/logging:log-error (format nil "Error handling message: ~a" e))
           (make-error-response nil
                                -32603
                                (princ-to-string e)))))

(defun handle-request (id method params)
  "Handle JSON-RPC 2.0 request.
Routes to appropriate handler based on METHOD string."
  (let ((*request-id* id))
    (cond
     ;; Core protocol
     ((string= method "initialize")
      (handle-initialize id params))
     ((string= method "ping")
      (handle-ping id))
     ;; Tools
     ((string= method "tools/list")
      (handle-tools-list id))
     ((string= method "tools/call")
      (handle-tool-call id params))
     ;; Resources (documentation exposure)
     ((string= method "resources/list")
      (handle-resources-list id))
     ((string= method "resources/read")
      (handle-resources-read id params))
     ;; Prompts (guided workflows)
     ((string= method "prompts/list")
      (handle-prompts-list id))
     ((string= method "prompts/get")
      (handle-prompts-get id params))
     ;; Approval (server-enforced user approval)
     ((string= method "approval/respond")
      (handle-approval-respond id params))
     ;; Unknown method
     (t (make-error-response id
                             -32601
                             (format nil "Unknown method: ~a" method))))))

(defun handle-notification (method params)
  "Handle JSON-RPC 2.0 notification.
Notifications have no ID and expect no response.
Logged via log4cl (not stdout) so stdio transport stays clean."
  (cl-tron-mcp/logging:log-info (format nil "Notification: ~a ~a" method params))
  nil)

(provide :cl-tron-mcp/protocol-handlers)
