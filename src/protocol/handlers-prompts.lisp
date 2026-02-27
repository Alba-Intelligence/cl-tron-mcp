;;;; src/protocol/handlers-prompts.lisp
;;;;
;;;; Prompt-related JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - prompts/list handler
;;;;   - prompts/get handler

(in-package :cl-tron-mcp/protocol)

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
          (make-error-response id
                               -32602
                               (getf validation :error)))))
    (handler-case (let ((response (cl-tron-mcp/prompts:handle-prompts-get id
                                                                       params)))
                    (jonathan:to-json response))
      (error (e)
             (cl-tron-mcp/logging:log-error (format nil "Error getting prompt: ~a" e))
             (make-error-response id
                                  -32000
                                  (princ-to-string e))))))

(provide :cl-tron-mcp/protocol-handlers-prompts)
