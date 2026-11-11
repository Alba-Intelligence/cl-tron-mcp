;;;; src/tools/handlers-tools.lisp
;;;;
;;;; Tool-related JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - tools/list handler
;;;;   - tools/call handler with approval workflow
;;;;   - approval/respond handler
;;;;   - Helper functions for tool execution and approval

(in-package :cl-tron-mcp/tools)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun arguments-without-approval-params (arguments)
  "Return arguments plist without approval_request_id and approved (for passing to tool handler)."
  (loop for (k v) on arguments by
        #'cddr
        when
        (and (not (eq k :|approval_request_id|))
             (not (eq k :|approved|)))
        append (list k v)))

(defun approval-binding (tool-name arguments)
  "Return a stable string fingerprint of TOOL-NAME plus ARGUMENTS (approval params excluded).
An approval is valid only for the exact call it was requested for."
  (with-standard-io-syntax
    (let ((*package* (find-package :keyword)))
      (prin1-to-string (list (string tool-name)
                             (arguments-without-approval-params arguments))))))

(defun check-tool-approval (tool-name arguments id)
  "Check if tool requires approval and handle accordingly.
Returns approval_required response if approval needed, nil otherwise."
  (when (and (tool-requires-user-approval-p
              tool-name)
             (not (cl-tron-mcp/security:whitelist-check-tool
                   tool-name arguments)))
    (let ((request (cl-tron-mcp/security:request-approval
                    (or (cl-tron-mcp/security:tool-name-to-operation
                         tool-name)
                        :eval)
                    (list :tool tool-name
                          :arguments arguments)
                    :binding (approval-binding tool-name arguments))))
      (return-from check-tool-approval
        (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
                                :|id| id
                                :|result| (list :|content| (list (list :|type| "text"
                                                                       :|text| (cl-tron-mcp/json-compat:to-json (list :|approval_required| t
                                                                                                       :|request_id| (cl-tron-mcp/security:approval-request-id
                                                                                                                      request)
                                                                                                       :|message|
                                                                                                       (if cl-tron-mcp/security:*require-confirmation-code*
                                                                                                           (format nil "User approval required for tool: ~a. Ask the user to read the confirmation code shown on the Tron server terminal, then call approval/respond with request_id, approved and confirmation_code. The code is never included in tool results."
                                                                                                                   tool-name)
                                                                                                           (format nil "User approval required for tool: ~a"
                                                                                                                   tool-name))))))))))))
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
        ((cl-tron-mcp/security:consume-approved-request-id
          (string approval-request-id)
          (approval-binding tool-name arguments))
         (return-from handle-approval-reinvocation
           (handle-tool-call-run tool-name
                                 (arguments-without-approval-params arguments)
                                 id)))
        (t (return-from handle-approval-reinvocation
             (make-error-response id -32001 "Approval expired or already used. Invoke the tool again to request a new approval."))))))
  nil)

;;; ------------------------------------------------------------
;;; Tool results as JSON (B7)
;;; ------------------------------------------------------------

(defun json-key-name (key)
  "Lowercase snake_case JSON key for the Lisp keyword KEY (:in-debugger -> \"in_debugger\")."
  (substitute #\_ #\- (string-downcase (symbol-name key))))

(defun boolean-key-name-p (name)
  "True when the JSON key NAME conventionally holds a flag, so NIL under it means false, not []."
  (or (member name '("success" "connected" "enabled" "running" "found" "valid" "available"
                     "ok" "error" "approved" "recorded" "changed" "removed" "cleared"
                     "requires_approval" "approval_required")
              :test #'string=)
      (let ((n (length name)))
        (or (and (> n 2) (string= "_p" name :start2 (- n 2)))
            (some (lambda (prefix) (and (> n (length prefix)) (string= prefix name :end2 (length prefix))))
                  '("is_" "has_" "in_" "can_" "was_"))))))

(defun result-plist-p (x)
  (and (consp x)
       (null (cdr (last x)))
       (evenp (length x))
       (loop for (k) on x by #'cddr always (keywordp k))))

(defun tool-result->json (obj &optional key-name)
  "Convert a Lisp tool result to a tree that CL-TRON-MCP/JSON-COMPAT:TO-JSON encodes as JSON with
lowercase snake_case keys. NIL under a flag-like key (see BOOLEAN-KEY-NAME-P) becomes false,
elsewhere an empty array. Values with no JSON form are printed."
  (cond
    ((eq obj t) t)
    ((null obj) (if (and key-name (boolean-key-name-p key-name)) :false nil))
    ((stringp obj) obj)
    ((numberp obj) (if (rationalp obj) (if (integerp obj) obj (float obj 1d0)) obj))
    ((keywordp obj) (string-downcase (symbol-name obj)))
    ((symbolp obj) (string-downcase (symbol-name obj)))
    ((characterp obj) (string obj))
    ((hash-table-p obj) obj)
    ((result-plist-p obj)
     (loop for (k v) on obj by #'cddr
           for name = (json-key-name k)
           append (list (intern name :keyword) (tool-result->json v name))))
    ((and (consp obj) (null (cdr (last obj))))
     (mapcar (lambda (x) (tool-result->json x nil)) obj))
    ((and (vectorp obj) (not (stringp obj)))
     (map 'list (lambda (x) (tool-result->json x nil)) obj))
    (t (let ((*print-pretty* nil)) (princ-to-string obj)))))

(defun tool-result-error-p (result)
  "True when RESULT is a plist carrying a true :error entry."
  (and (result-plist-p result) (getf result :error) t))

;;; ------------------------------------------------------------
;;; Bounded tool execution (B4)
;;; ------------------------------------------------------------

(defun call-tool-bounded (tool-name arguments seconds)
  "Run CALL-TOOL in a worker thread and wait at most SECONDS. Returns the tool's value; re-signals
an error raised by the tool in the calling thread; signals TIMEOUT-ERROR and terminates the worker
if the deadline passes. The wait polls a flag (10 ms) instead of using CONDITION-WAIT :TIMEOUT,
whose lock state after a timed-out wait differs between implementations."
  (let* ((lock (bordeaux-threads:make-lock "tool-call"))
         (done nil)
         (outcome nil)
         (thread (bordeaux-threads:make-thread
                  (lambda ()
                    (let ((result (handler-case (list :ok (call-tool tool-name arguments))
                                    (error (e) (list :error e)))))
                      (bordeaux-threads:with-lock-held (lock)
                        (setf outcome result done t))))
                  :name (format nil "tool ~a" tool-name)))
         (deadline (+ (get-internal-real-time)
                      (* seconds internal-time-units-per-second))))
    (loop
      (when (bordeaux-threads:with-lock-held (lock) done) (return))
      (when (>= (get-internal-real-time) deadline)
        (ignore-errors (bordeaux-threads:destroy-thread thread))
        (error 'timeout-error
               :message (format nil "Tool execution timeout after ~d seconds" seconds)))
      (sleep 0.01))
    (ecase (first outcome)
      (:ok (second outcome))
      (:error (error (second outcome))))))

(defun execute-tool-with-timeout (tool-name arguments id)
  "Execute tool with timeout, metrics recording, and request tracing.
Returns JSON-RPC response with result or error."
  (let ((start-tick (get-internal-real-time))
        (timeout-seconds *default-tool-timeout*)
        (result nil)
        (error-occurred nil))
    (cl-tron-mcp/monitor:trace-log "executing tool ~a" tool-name)
    (unwind-protect
         (handler-case (progn
                         ;; Execute tool in a worker with a bounded wait
                         (setf result (call-tool-bounded tool-name arguments timeout-seconds))
                         ;; Return success response; error plists are flagged with isError
                         (cl-tron-mcp/json-compat:to-json
                          (list :|jsonrpc| "2.0"
                                :|id| id
                                :|result| (nconc
                                           (list :|content|
                                                 (list (list :|type| "text"
                                                             :|text| (cl-tron-mcp/json-compat:to-json
                                                                      (tool-result->json result)))))
                                           (when (tool-result-error-p result)
                                             (list :|isError| t))))))
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
      ;; Record metrics
      (let ((latency-ms (round (* 1000 (/ (- (get-internal-real-time) start-tick)
                                          internal-time-units-per-second)))))
        (cl-tron-mcp/tracer:metrics-record-call tool-name latency-ms :error-p error-occurred))
      ;; Trace completion
      (cl-tron-mcp/monitor:trace-log "tool ~a done (error=~a)" tool-name error-occurred)
      ;; If error occurred, perform additional cleanup
      (when error-occurred
        (cleanup-on-error (format nil "tool call: ~a" tool-name))))))

;;; ============================================================
;;; Tools Handlers
;;; ============================================================

(defun handle-tools-list (id)
  "Handle tools/list request.
Returns list of all available tools with their schemas."
  (let ((tools (list-tool-descriptors)))
    (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
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
  "Handle approval/respond request. Params: request_id (string), approved (boolean), confirmation_code (string, required to
   approve unless TRON_APPROVAL_TRUST_CLIENT is set), optional message.
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
                    (cl-tron-mcp/security:approval-response
                     (string request-id) response
                     :code (let ((code (getf params :|confirmation_code|)))
                             (and code (string code))))
                    (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
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

(provide :cl-tron-mcp/tools-handlers)
