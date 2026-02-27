;;;; src/prompts/handler.lisp
;;;;
;;;; Implementation of MCP Prompts for cl-tron-mcp.
;;;;
;;;; This file provides:
;;;;   - Prompt listing (prompts/list)
;;;;   - Prompt retrieval (prompts/get)
;;;;   - Infrastructure for prompt registry
;;;;
;;;; Individual prompt definitions are in src/prompts/individual/
;;;;
;;;; Usage:
;;;;   MCP clients call prompts/list to see available workflows
;;;;   MCP clients call prompts/get with a prompt name to get instructions

(in-package :cl-tron-mcp/prompts)

;;; ============================================================
;;; Condition Types
;;; ============================================================

(define-condition prompt-not-found-error (error)
  ((name :initarg :name :reader prompt-not-found-name))
  (:report (lambda (condition stream)
             (format stream "Prompt not found: ~a"
                     (prompt-not-found-name condition)))))

;;; ============================================================
;;; Prompt Registry
;;; ============================================================

(defvar *prompt-registry* (make-hash-table :test 'equal)
  "Registry of available prompts, keyed by name (string).")

(defstruct prompt-descriptor
  "Structure representing an MCP prompt."
  name
  title
  description
  arguments
  messages)

(defun define-prompt (name title description arguments messages)
  "Register a prompt in the registry.
NAME is the prompt identifier (string).
TITLE is a human-readable title.
DESCRIPTION explains what the prompt does.
ARGUMENTS is a list of argument specifications (can be nil).
MESSAGES is a list of message content (the prompt text)."
  (setf (gethash name *prompt-registry*)
        (make-prompt-descriptor
         :name name
         :title title
         :description description
         :arguments arguments
         :messages messages)))


;;; ============================================================
;;; Prompt Listing (prompts/list)
;;; ============================================================

(defun list-prompts ()
  "Return a list of all registered prompts.
Implements the MCP prompts/list operation."
  (loop for prompt being the hash-values of *prompt-registry*
        collect prompt))

(defun handle-prompts-list (id)
  "Handle MCP prompts/list request.
Returns plist suitable for JSON serialization."
  (let ((prompts (list-prompts)))
    (list :|jsonrpc| "2.0"
          :|id| id
          :|result|
          (list :|prompts|
                (loop for prompt in prompts
                      collect (list :|name| (prompt-descriptor-name prompt)
                                    :|title| (prompt-descriptor-title prompt)
                                    :|description| (prompt-descriptor-description prompt)
                                    :|arguments| (prompt-descriptor-arguments prompt)))))))

;;; ============================================================
;;; Prompt Retrieval (prompts/get)
;;; ============================================================
;;; MCP spec: each message has role (e.g. "user") and content as an array of
;;; parts: [ { "type": "text", "text": "..." } ], not a single content object.
;;; Part keys must be lowercase in JSON (use :|type| :|text|).

(defun normalize-part (part)
  "Return part with lowercase keys for JSON (:type -> :|type|, :text -> :|text|)."
  (list :|type| (or (getf part :|type|) (getf part :type) "text")
        :|text| (or (getf part :|text|) (getf part :text) "")))

(defun message-content-to-parts (content)
  "Ensure content is a list of part objects (MCP parts array). If CONTENT is a single part object, wrap in list and normalize keys."
  (if (and (listp content) (not (null content))
           (or (eq (first content) :type) (eq (first content) :|type|)))
      (list (normalize-part content))
      (mapcar #'normalize-part content)))

(defun normalize-prompt-message (message)
  "Return message with content as array of parts for MCP spec."
  (list :|role| (getf message :role)
        :|content| (message-content-to-parts (getf message :content))))

(defun get-prompt (name)
  "Get a prompt by name.
Returns the prompt descriptor or signals PROMPT-NOT-FOUND-ERROR."
  (let ((prompt (gethash name *prompt-registry*)))
    (unless prompt
      (error 'prompt-not-found-error :name name))
    prompt))

(defun handle-prompts-get (id params)
  "Handle MCP prompts/get request.
PARAMS should contain :|name| with the prompt name.
Returns plist suitable for JSON serialization. Messages use content as parts array per MCP spec."
  (let ((name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (declare (ignore arguments))
    (handler-case
        (let ((prompt (get-prompt name)))
          (list :|jsonrpc| "2.0"
                :|id| id
                :|result|
                (list :|description| (prompt-descriptor-description prompt)
                      :|messages| (mapcar #'normalize-prompt-message (prompt-descriptor-messages prompt)))))
      (prompt-not-found-error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32602
                    :|message| (format nil "Prompt not found: ~a"
                                       (prompt-not-found-name e)))))
      (error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32603
                    :|message| (princ-to-string e)))))))

;;; ============================================================
;;; Help
;;; ============================================================

(defun prompts-help ()
  "Return help text for the prompts module."
  (list :module "cl-tron-mcp/prompts"
        :description "MCP Prompts implementation for guided workflows"
        :operations (list
                    (list :name "prompts/list"
                          :description "List all available guided workflows")
                    (list :name "prompts/get"
                          :description "Get a specific workflow by name"))
        :available-prompts (loop for name being the hash-keys of *prompt-registry*
                                 collect name)))

(provide :cl-tron-mcp/prompts)
