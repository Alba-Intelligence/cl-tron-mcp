;;;; src/protocol/handlers-resources.lisp
;;;;
;;;; Resource-related JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - resources/list handler
;;;;   - resources/read handler

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Resources Handlers
;;; ============================================================

(defun handle-resources-list (id)
  "Handle resources/list request.
Returns list of documentation files that can be read.
These resources help AI agents understand how to use the MCP server."
  (let ((response (cl-tron-mcp/resources:handle-resources-list
                   id)))
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
          (make-error-response id
                               -32602
                               (getf validation :error)))))
    (handler-case (let ((response (cl-tron-mcp/resources:handle-resources-read
                                   id params)))
                    (jonathan:to-json response))
      (error (e)
             (cl-tron-mcp/logging:log-error (format nil "Error reading resource: ~a" e))
             (make-error-response id
                                  -32000
                                  (princ-to-string e))))))

(provide :cl-tron-mcp/protocol-handlers-resources)
