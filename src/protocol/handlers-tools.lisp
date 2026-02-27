;;;; src/protocol/handlers-tools.lisp
;;;;
;;;; Tool-related JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - tools/list handler
;;;;   - tools/call handler with approval workflow
;;;;   - approval/respond handler
;;;;   - Helper functions for tool execution and approval

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun arguments-without-approval-params (arguments)
  "Return arguments plist without approval_request_id and approved (for passing to tool handler)."
  (loop for
        (k v)
        on
        arguments
        by
        #'cddr
        when
        (and (not (eq k :|approval_request_id|))
             (not (eq k :|approved|)))
        append
        (list k v)))

(defun check-tool-approval (tool-name arguments id)
  "Check if tool requires approval and handle accordingly.
Returns approval_required response if approval needed, nil otherwise."
  (when (and (cl-tron-mcp/tools:tool-requires-user-approval-p
              tool-name)
             (not (cl-tron-mcp/security:whitelist-check-tool
                   tool-name arguments)))
    (let ((request (cl-tron-mcp/security:request-approval 
                    (or (cl-tron-mcp/security:tool-name-to-operation
                         tool-name)
                        :eval)
                    (list :tool tool-name
                          :arguments arguments))))
      (return-from check-tool-approval
        (jonathan:to-json (list :|jsonrpc| "2.0"
                                :|id| id
                                :|result| (list :|content| (list (list :|type| "text"
                                                                       :|text| (jonathan:to-json (list :|approval_required| t
                                                                                                       :|request_id| (cl-tron-mcp/security:approval-request-id
                                                                                                                      request)
                                                                                                       :|message|
                                                                                                       (format nil "User approval required for tool: ~a"
                                                                                                               tool-name)))))))))))
  nil)

(defun handle-approval-reinvocation (tool-name arguments id)
  "Handle re-invocation with approval from client.
Returns tool result if approval valid, error if expired/used.
Returns nil if no approval params present."
  (let ((approval-request-id (getf arguments :|approval_request_id|))
        (approved (getf arguments :|approved|)))
    (when (and approval-request-id
               approved
               (or (eq approved t)
                   (string= (string approved)
                            "true")))
      (cond
       ((cl-tron-mcp/security:consume-approved-request-id (string approval-request-id))
        (return-from handle-approval-reinvocation
          (handle-tool-call-run tool-name
                                (arguments-without-approval-params arguments)
                                id)))
       (t (return-from handle-approval-reinvocation
            (make-error-response id -32001 "Approval expired or already used. Invoke the tool again to request a new approval."))))))
  nil)

(defun execute-tool-with-timeout (tool-name arguments id)
  "Execute tool with timeout and error handling.
Returns JSON-RPC response with result or error."
  (let ((start-time (get-universal-time))
        (timeout-seconds *default-tool-timeout*)
        (result nil)
        (error-occurred nil))
    (unwind-protect
        (handler-case (progn
                        ;; Track this request for cleanup
                        (bordeaux-threads:with-lock-held (*request-lock*)
                                                         (setf (gethash id *pending-requests*) t))
                        ;; Check timeout before executing
                        (let ((elapsed (- (get-universal-time)
                                          start-time)))
                          (when (>= elapsed timeout-seconds)
                            (error 'timeout-error
                                   :message (format nil "Tool execution timeout after ~d seconds"
                                                    timeout-seconds))))
                        ;; Execute tool
                        (setf result (cl-tron-mcp/tools:call-tool tool-name arguments))
                        ;; Check timeout after execution
                        (let ((elapsed (- (get-universal-time)
                                          start-time)))
                          (when (>= elapsed timeout-seconds)
                            (error 'timeout-error
                                   :message (format nil "Tool execution timeout after ~d seconds"
                                                    timeout-seconds))))
                        ;; Return success response
                        (jonathan:to-json (list :|jsonrpc| "2.0"
                                                :|id| id
                                                :|result| (list :|content| (list (list :|type| "text"
                                                                                       :|text| (format nil "~a" result)))))))
          (timeout-error (e)
                         (setf error-occurred t)
                         (cl-tron-mcp/logging:log-warn (format nil "Tool ~a timed out after ~d seconds"
                                                               tool-name timeout-seconds))
                         (make-error-response id
                                              -32008
                                              (timeout-error-message e)))
          (error (e)
                 (setf error-occurred t)
                 (cl-tron-mcp/logging:log-error (format nil "Error executing tool ~a: ~a"
                                                        tool-name e))
                 (make-error-response id
                                      -32000
                                      (princ-to-string e))))
      ;; Cleanup: remove from pending requests
      (bordeaux-threads:with-lock-held (*request-lock*)
                                       (remhash id *pending-requests*))
      ;; If error occurred, perform additional cleanup
      (when error-occurred
        (cleanup-on-error (format nil "tool call: ~a" tool-name))))))

;;; ============================================================
;;; Tools Handlers
;;; ============================================================

(defun handle-tools-list (id)
  "Handle tools/list request.
Returns list of all available tools with their schemas."
  (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
    (jonathan:to-json (list :|jsonrpc| "2.0"
                            :|id| id
                            :|result| (list :|tools| tools)))))

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
          (make-error-response id
                               -32602
                               (getf validation :error)))))
    ;; Validate arguments is a list if provided
    (when arguments
      (let ((validation (validate-list-param "arguments" arguments)))
        (unless (getf validation :valid)
          (return-from handle-tool-call
            (make-error-response id
                                 -32602
                                 (getf validation :error))))))
    (handler-case (progn
                    ;; Check for approval re-invocation
                    (let ((approval-result (handle-approval-reinvocation tool-name arguments id)))
                      (when approval-result
                        (return-from handle-tool-call approval-result)))
                    ;; Check if tool requires approval
                    (let ((approval-required (check-tool-approval tool-name arguments id)))
                      (when approval-required
                        (return-from handle-tool-call approval-required)))
                    ;; Run tool (no approval needed or whitelisted)
                    (execute-tool-with-timeout tool-name
                                               (or arguments
                                                   (list))
                                               id))
      (error (e)
             (cleanup-on-error (format nil "tool call: ~a" tool-name)
                               e)
             (make-error-response id
                                  -32000
                                  (princ-to-string e))))))

(defun handle-tool-call-run (tool-name arguments id)
  "Helper: run tool and return JSON-RPC result with timeout and cleanup.
DEPRECATED: Use execute-tool-with-timeout instead."
  (execute-tool-with-timeout tool-name arguments id))

(defun handle-approval-respond (id params)
  "Handle approval/respond request. Params: request_id (string), approved (boolean), optional message.
   Records the user decision; client then re-invokes the tool with approval_request_id and approved: true."
  (let ((request-id (getf params :|request_id|))
        (approved (getf params :|approved|))
        (message (getf params :|message|)))
    ;; Validate request_id
    (let ((validation (validate-string-param "request_id" request-id
                                             :required t)))
      (unless (getf validation :valid)
        (return-from handle-approval-respond
          (make-error-response id
                               -32602
                               (getf validation :error)))))
    ;; Validate approved is boolean or string
    (when (and approved
               (not (or (eq approved t)
                        (eq approved nil)
                        (and (stringp approved)
                             (or (string= approved "true")
                                 (string= approved "false"))))))
      (return-from handle-approval-respond
        (make-error-response id -32602 "approved must be a boolean or 'true'/'false' string")))
    (handler-case (let ((response (if (or (eq approved t)
                                           (and approved
                                                (string= (string approved)
                                                         "true")))
                                       :approved :denied)))
                    (cl-tron-mcp/security:approval-response (string request-id)
                                                            response)
                    (jonathan:to-json (list :|jsonrpc| "2.0"
                                            :|id| id
                                            :|result| (nconc (list :|recorded| t
                                                                   :|approved| (eq response :approved))
                                                              (when (eq response :denied)
                                                                (list :|message| (or (when message
                                                                                       (string message))
                                                                                     "User denied approval. You can retry by invoking the tool again.")))))))
      (error (e)
             (cleanup-on-error (format nil "approval respond: ~a" request-id)
                               e)
             (make-error-response id
                                  -32000
                                  (princ-to-string e))))))

(provide :cl-tron-mcp/protocol-handlers-tools)
