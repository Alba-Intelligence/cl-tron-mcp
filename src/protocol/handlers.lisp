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

(defvar *default-tool-timeout* 30
  "Default timeout for tool execution in seconds.")

(defvar *pending-requests* (make-hash-table :test 'eql)
  "Hash table tracking pending requests for cleanup.")

(defvar *request-lock* (bordeaux-threads:make-lock "pending-requests")
  "Lock for synchronizing access to *pending-requests*.")

;;; ============================================================
;;; Error Recovery and Cleanup
;;; ============================================================

(defun cleanup-on-error (error-context &optional (error-condition nil))
  "Perform cleanup when an error occurs.
ERROR-CONTEXT is a string describing what operation failed.
ERROR-CONDITION is the actual condition object (optional)."
  (handler-case
      (progn
        (cl-tron-mcp/logging:log-error
         (format nil "Error in ~a: ~a"
                 error-context
                 (if error-condition
                     (princ-to-string error-condition)
                     "unknown")))
        ;; Disconnect from Swank if connected
        (when (cl-tron-mcp/swank:swank-connected-p)
          (cl-tron-mcp/logging:log-info
           (format nil "Disconnecting from Swank due to error in ~a"
                   error-context))
          (cl-tron-mcp/swank:swank-disconnect))
        ;; Clear pending requests
        (bordeaux-threads:with-lock-held (*request-lock*)
          (clrhash *pending-requests*))
        (cl-tron-mcp/logging:log-info
         (format nil "Cleanup completed for error in ~a" error-context)))
    (error (e)
      (cl-tron-mcp/logging:log-error
       (format nil "Error during cleanup: ~a" e)))))

(defun validate-string-param (param-name value &key required (min-length 1))
  "Validate a string parameter.
Returns T if valid, otherwise returns an error response plist."
  (when (and required (null value))
    (return-from validate-string-param
      (list :valid nil :error (format nil "Missing required parameter: ~a" param-name))))
  (when (and value (not (stringp value)))
    (return-from validate-string-param
      (list :valid nil :error (format nil "Parameter ~a must be a string" param-name))))
  (when (and value (stringp value) (< (length value) min-length))
    (return-from validate-string-param
      (list :valid nil :error (format nil "Parameter ~a must be at least ~d characters" param-name min-length))))
  (list :valid t))

(defun validate-list-param (param-name value &key required)
  "Validate a list parameter.
Returns T if valid, otherwise returns an error response plist."
  (when (and required (null value))
    (return-from validate-list-param
      (list :valid nil :error (format nil "Missing required parameter: ~a" param-name))))
  (when (and value (not (listp value)))
    (return-from validate-list-param
      (list :valid nil :error (format nil "Parameter ~a must be a list" param-name))))
  (list :valid t))

(defun validate-integer-param (param-name value &key required (min 0))
  "Validate an integer parameter.
Returns T if valid, otherwise returns an error response plist."
  (when (and required (null value))
    (return-from validate-integer-param
      (list :valid nil :error (format nil "Missing required parameter: ~a" param-name))))
  (when (and value (not (integerp value)))
    (return-from validate-integer-param
      (list :valid nil :error (format nil "Parameter ~a must be an integer" param-name))))
  (when (and value (integerp value) (< value min))
    (return-from validate-integer-param
      (list :valid nil :error (format nil "Parameter ~a must be >= ~d" param-name min))))
  (list :valid t))

(defmacro with-timeout ((seconds) &body body)
  "Execute BODY with a timeout of SECONDS seconds.
If timeout occurs, a TIMEOUT-ERROR condition is signaled."
  (let ((timeout-tag (gensym "TIMEOUT-TAG-")))
    `(handler-case
         (let ((start-time (get-universal-time)))
           (unwind-protect
                (progn ,@body)
             (when (> (- (get-universal-time) start-time) ,seconds)
               (error 'timeout-error :message "Operation timed out"))))
       (timeout-error (e)
         (declare (ignore e))
         (error 'timeout-error :message (format nil "Operation timed out after ~d seconds" ,seconds))))))

(define-condition timeout-error (error)
  ((message :initarg :message :reader timeout-error-message))
  (:report (lambda (condition stream)
             (format stream "Timeout: ~a" (timeout-error-message condition)))))

(defun get-unix-time ()
  "Get current Unix timestamp (seconds since epoch)."
  #+sbcl
  (sb-ext:get-time-of-day)
  #+ccl
  (ccl:get-time-of-day)
  #-(or sbcl ccl)
  (- (/ (get-universal-time) 86400) 2))

;;; ============================================================
;;; Message Dispatch
;;; ============================================================

(defun handle-message (message)
  "Handle incoming JSON-RPC message.
Dispatches to appropriate handler based on method name.
Messages without ID are treated as notifications."
  (handler-case
      (let* ((parsed (if (stringp message) (jonathan:parse message) message))
             (id (getf parsed :|id|))
             (method (getf parsed :|method|))
             (params (getf parsed :|params|)))
        (cond
          ((null id)
           (handle-notification method params))
          (t
           (handle-request id method params))))
    (jonathan.error:<jonathan-error> (e)
      (cl-tron-mcp/logging:log-error (format nil "JSON parse error: ~a" e))
      (make-error-response nil -32700 "Parse error"))
    (error (e)
      (cl-tron-mcp/logging:log-error (format nil "Error handling message: ~a" e))
      (make-error-response nil -32603 (princ-to-string e)))))

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
This is the first message sent by MCP clients during handshake.
Response is used by both stdio and HTTP transports unchanged."
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
                ;; Resources capability - exposes documentation files (subscribe must be boolean per MCP; nil encodes as [] in Jonathan)
                :|resources| (list :|subscribe| :false :|listChanged| t)
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

(defun arguments-without-approval-params (arguments)
  "Return arguments plist without approval_request_id and approved (for passing to tool handler)."
  (loop for (k v) on arguments by #'cddr
        when (and (not (eq k :|approval_request_id|)) (not (eq k :|approved|)))
          append (list k v)))

(defun handle-tool-call (id params)
  "Handle tools/call request.
   If tool requires user approval: whitelist -> run; else return approval_required.
   If arguments include approval_request_id and approved: true, consume and run tool.
   Denial or invalid approval returns error with message (retry = same tool again)."
  (let ((tool-name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    ;; Validate tool-name
    (let ((validation (validate-string-param "name" tool-name :required t)))
      (unless (getf validation :valid)
        (return-from handle-tool-call
          (make-error-response id -32602 (getf validation :error)))))
    ;; Validate arguments is a list if provided
    (when arguments
      (let ((validation (validate-list-param "arguments" arguments)))
        (unless (getf validation :valid)
          (return-from handle-tool-call
            (make-error-response id -32602 (getf validation :error))))))
    (handler-case
        (progn
          ;; Re-invocation with approval from client (Option A)
          (let ((approval-request-id (getf arguments :|approval_request_id|))
                (approved (getf arguments :|approved|)))
            (when (and approval-request-id approved
                       (or (eq approved t) (string= (string approved) "true")))
              (cond
                ((cl-tron-mcp/security:consume-approved-request-id
                  (string approval-request-id))
                 (return-from handle-tool-call
                   (handle-tool-call-run tool-name (arguments-without-approval-params arguments) id)))
                (t
                 (return-from handle-tool-call
                   (make-error-response id -32001
                                        "Approval expired or already used. Invoke the tool again to request a new approval."))))))
          ;; Tool requires user approval and no whitelist -> return approval_required
          (when (and (cl-tron-mcp/tools:tool-requires-user-approval-p tool-name)
                     (not (cl-tron-mcp/security:whitelist-check-tool tool-name arguments)))
            (let ((request (cl-tron-mcp/security:request-approval
                            (or (cl-tron-mcp/security:tool-name-to-operation tool-name) :eval)
                            (list :tool tool-name :arguments arguments))))
              (return-from handle-tool-call
                (jonathan:to-json
                 (list :|jsonrpc| "2.0"
                       :|id| id
                       :|result|
                       (list :|content|
                             (list (list :|type| "text"
                                         :|text| (jonathan:to-json
                                                  (list :|approval_required| t
                                                        :|request_id| (cl-tron-mcp/security:approval-request-id request)
                                                        :|message| (format nil "User approval required for tool: ~a" tool-name))))))))))
            ;; Run tool (no approval needed or whitelisted)
            (handle-tool-call-run tool-name (or arguments (list)) id))
          (error (e)
                 (cleanup-on-error (format nil "tool call: ~a" tool-name) e)
                 (make-error-response id -32000 (princ-to-string e)))))))

(defun handle-tool-call-run (tool-name arguments id)
  "Helper: run tool and return JSON-RPC result with timeout and cleanup."
  (let ((start-time (get-universal-time))
        (timeout-seconds *default-tool-timeout*)
        (result nil)
        (error-occurred nil))
    (unwind-protect
         (handler-case
             (progn
               ;; Track this request for cleanup
               (bordeaux-threads:with-lock-held (*request-lock*)
                 (setf (gethash id *pending-requests*) t))
               ;; Check timeout before executing
               (let ((elapsed (- (get-universal-time) start-time)))
                 (when (>= elapsed timeout-seconds)
                   (error 'timeout-error
                          :message (format nil "Tool execution timeout after ~d seconds" timeout-seconds))))
               ;; Execute tool
               (setf result (cl-tron-mcp/tools:call-tool tool-name arguments))
               ;; Check timeout after execution
               (let ((elapsed (- (get-universal-time) start-time)))
                 (when (>= elapsed timeout-seconds)
                   (error 'timeout-error
                          :message (format nil "Tool execution timeout after ~d seconds" timeout-seconds))))
               ;; Return success response
               (jonathan:to-json
                (list :|jsonrpc| "2.0"
                      :|id| id
                      :|result|
                      (list :|content|
                            (list (list :|type| "text"
                                        :|text| (format nil "~a" result)))))))
           (timeout-error (e)
             (setf error-occurred t)
             (cl-tron-mcp/logging:log-warn
              (format nil "Tool ~a timed out after ~d seconds"
                      tool-name timeout-seconds))
             (make-error-response id -32008 (timeout-error-message e)))
           (error (e)
             (setf error-occurred t)
             (cl-tron-mcp/logging:log-error
              (format nil "Error executing tool ~a: ~a" tool-name e))
             (make-error-response id -32000 (princ-to-string e))))
      ;; Cleanup: remove from pending requests
      (bordeaux-threads:with-lock-held (*request-lock*)
        (remhash id *pending-requests*))
      ;; If error occurred, perform additional cleanup
      (when error-occurred
        (cleanup-on-error (format nil "tool call: ~a" tool-name))))))

(defun handle-approval-respond (id params)
  "Handle approval/respond request. Params: request_id (string), approved (boolean), optional message.
   Records the user decision; client then re-invokes the tool with approval_request_id and approved: true."
  (let ((request-id (getf params :|request_id|))
        (approved (getf params :|approved|))
        (message (getf params :|message|)))
    ;; Validate request_id
    (let ((validation (validate-string-param "request_id" request-id :required t)))
      (unless (getf validation :valid)
        (return-from handle-approval-respond
          (make-error-response id -32602 (getf validation :error)))))
    ;; Validate approved is boolean or string
    (when (and approved (not (or (eq approved t) (eq approved nil)
                                 (and (stringp approved)
                                      (or (string= approved "true") (string= approved "false"))))))
      (return-from handle-approval-respond
        (make-error-response id -32602 "approved must be a boolean or 'true'/'false' string")))
    (handler-case
        (let ((response (if (or (eq approved t) (and approved (string= (string approved) "true")))
                            :approved
                            :denied)))
          (cl-tron-mcp/security:approval-response (string request-id) response)
          (jonathan:to-json
           (list :|jsonrpc| "2.0"
                 :|id| id
                 :|result| (nconc (list :|recorded| t :|approved| (eq response :approved))
                                  (when (eq response :denied)
                                    (list :|message| (or (when message (string message))
                                                         "User denied approval. You can retry by invoking the tool again.")))))))
      (error (e)
        (cleanup-on-error (format nil "approval respond: ~a" request-id) e)
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
  ;; Validate uri parameter
  (let ((uri (getf params :|uri|)))
    (let ((validation (validate-string-param "uri" uri :required t)))
      (unless (getf validation :valid)
        (return-from handle-resources-read
          (make-error-response id -32602 (getf validation :error)))))
    (handler-case
        (let ((response (cl-tron-mcp/resources:handle-resources-read id params)))
          (jonathan:to-json response))
      (error (e)
        (cl-tron-mcp/logging:log-error (format nil "Error reading resource: ~a" e))
        (make-error-response id -32000 (princ-to-string e))))))

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
  ;; Validate name parameter
  (let ((name (getf params :|name|)))
    (let ((validation (validate-string-param "name" name :required t)))
      (unless (getf validation :valid)
        (return-from handle-prompts-get
          (make-error-response id -32602 (getf validation :error)))))
    (handler-case
        (let ((response (cl-tron-mcp/prompts:handle-prompts-get id params)))
          (jonathan:to-json response))
      (error (e)
        (cl-tron-mcp/logging:log-error (format nil "Error getting prompt: ~a" e))
        (make-error-response id -32000 (princ-to-string e))))))

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
