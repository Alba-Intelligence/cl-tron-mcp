;;;; src/protocol/handlers.lisp
;;;;
;;;; JSON-RPC 2.0 request handlers for MCP protocol.
;;;;
;;;; This file implements the core MCP protocol handlers:
;;;;   - initialize     : Server handshake and capability negotiation
;;;;   - tools/list     : List available tools
;;;;   - tools/call     : Invoke a tool
;;;;   - resources/list : List available resources (documentation files)
;;;;   - resources/read : Read a resource by URI
;;;;   - prompts/list   : List available prompts (guided workflows)
;;;;   - prompts/get    : Get a prompt by name
;;;;   - ping           : Keepalive
;;;;
;;;; Capability Declaration:
;;;;   The server declares the following capabilities:
;;;;   - tools         : Tool invocation support
;;;;   - resources     : Documentation exposure support
;;;;   - prompts       : Guided workflow support
;;;;
;;;; See: https://modelcontextprotocol.io/docs/concepts/tools
;;;; See: https://modelcontextprotocol.io/docs/concepts/resources
;;;; See: https://modelcontextprotocol.io/docs/concepts/prompts

(in-package :cl-tron-mcp/protocol)

(defvar *message-handler* nil)
(defvar *request-id* nil)

;;; ============================================================
;;; Message Dispatch
;;; ============================================================

(defun handle-message (message)
  "Handle incoming JSON-RPC message.
Dispatches to appropriate handler based on method name.
Messages without ID are treated as notifications."
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
      
      ;; Unknown method
      (t
       (make-error-response id -32601 (format nil "Unknown method: ~a" method))))))

(defun handle-notification (method params)
  "Handle JSON-RPC 2.0 notification.
Notifications have no ID and expect no response.
Logged via log4cl (not stdout) so stdio transport stays clean."
  (cl-tron-mcp/logging:log-info (format nil "Notification: ~a ~a" method params))
  nil)

;;; ============================================================
;;; Initialize Handler
;;; ============================================================

(defun handle-initialize (id params)
  "Handle initialize request.
Returns server capabilities including tools, resources, and prompts.
This is the first message sent by MCP clients during handshake."
  (declare (ignore params))
  (jonathan:to-json
   (list :|jsonrpc| "2.0"
         :|id| id
         :|result|
         (list :|protocolVersion| "2024-11-05"
               :|capabilities|
               (list
                ;; Tools capability - allows model-controlled tool invocation
                :|tools| (list :|listChanged| t)
                ;; Resources capability - exposes documentation files
                :|resources| (list :|subscribe| nil :|listChanged| t)
                ;; Prompts capability - exposes guided workflows
                :|prompts| (list :|listChanged| t))
               :|serverInfo|
               (list :|name| "cl-tron-mcp"
                     :|version| cl-tron-mcp/core:*version*)))))

;;; ============================================================
;;; Tools Handlers
;;; ============================================================

(defun handle-tools-list (id)
  "Handle tools/list request.
Returns list of all available tools with their schemas."
  (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
    (jonathan:to-json
     (list :|jsonrpc| "2.0"
           :|id| id
           :|result| (list :|tools| tools)))))

(defun handle-tool-call (id params)
  "Handle tools/call request.
Invokes the named tool with provided arguments."
  (let ((tool-name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (handler-case
        (let ((result (cl-tron-mcp/tools:call-tool tool-name arguments)))
          (jonathan:to-json
           (list :|jsonrpc| "2.0"
                 :|id| id
                 :|result|
                 (list :|content|
                       (list (list :|type| "text"
                                  :|text| (format nil "~a" result)))))))
      (error (e)
        (make-error-response id -32000 (princ-to-string e))))))

;;; ============================================================
;;; Resources Handlers
;;; ============================================================

(defun handle-resources-list (id)
  "Handle resources/list request.
Returns list of documentation files that can be read.
These resources help AI agents understand how to use the MCP server."
  (let ((response (cl-tron-mcp/resources:handle-resources-list id)))
    (jonathan:to-json response)))

(defun handle-resources-read (id params)
  "Handle resources/read request.
Returns contents of a specific documentation file by URI.
URI format: file://relative/path/to/file.md"
  (let ((response (cl-tron-mcp/resources:handle-resources-read id params)))
    (jonathan:to-json response)))

;;; ============================================================
;;; Prompts Handlers
;;; ============================================================

(defun handle-prompts-list (id)
  "Handle prompts/list request.
Returns list of guided workflows available to AI agents.
These prompts help agents understand the correct usage patterns."
  (let ((response (cl-tron-mcp/prompts:handle-prompts-list id)))
    (jonathan:to-json response)))

(defun handle-prompts-get (id params)
  "Handle prompts/get request.
Returns a specific prompt with step-by-step instructions.
PROMPT-NAME is passed in params."
  (let ((response (cl-tron-mcp/prompts:handle-prompts-get id params)))
    (jonathan:to-json response)))

;;; ============================================================
;;; Ping Handler
;;; ============================================================

(defun handle-ping (id)
  "Handle ping request.
Returns pong response for keepalive."
  (jonathan:to-json
   (list :|jsonrpc| "2.0"
         :|id| id
         :|result| (list :|pong| t))))

;;; ============================================================
;;; Error Response
;;; ============================================================

(defun make-error-response (id code message)
  "Create JSON-RPC error response.
CODE should be a standard JSON-RPC error code:
  -32600 : Invalid Request
  -32601 : Method Not Found
  -32602 : Invalid Params
  -32603 : Internal Error
  -32000 to -32099 : Server-defined errors"
  (jonathan:to-json
   (list :|jsonrpc| "2.0"
         :|id| id
         :|error| (list :|code| code
                       :|message| message))))

(provide :cl-tron-mcp/protocol-handlers)
