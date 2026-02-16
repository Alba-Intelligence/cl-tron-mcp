;;;; src/prompts/package.lisp
;;;;
;;;; MCP Prompts module for cl-tron-mcp
;;;;
;;;; This module implements the MCP Prompts protocol, allowing AI agents
;;;; to discover and use guided workflows for Common Lisp development.
;;;;
;;;; MCP Prompts Protocol:
;;;; =====================
;;;; Prompts in MCP are user-controlled templates that provide structured
;;;; instructions for interacting with the server. They appear as slash
;;;; commands in MCP clients.
;;;;
;;;; Protocol Methods:
;;;;   - prompts/list   : Returns list of available prompts
;;;;   - prompts/get    : Returns a specific prompt with instructions
;;;;
;;;; Available Prompts:
;;;;   - getting-started       : How to connect to Swank and verify setup
;;;;   - debugging-workflow    : Step-by-step error debugging
;;;;   - hot-reload-workflow   : Live code modification without restart
;;;;   - profiling-workflow    : Performance analysis workflow
;;;;
;;;; See: https://modelcontextprotocol.io/docs/concepts/prompts

(defpackage :cl-tron-mcp/prompts
  (:use :cl)
  (:nicknames :tron/prompts)
  (:export
   ;; Prompt listing
   #:list-prompts
   #:prompt-descriptor
   
   ;; Prompt retrieval
   #:get-prompt
   #:prompt-not-found-error
   
   ;; MCP handlers
   #:handle-prompts-list
   #:handle-prompts-get
   
   ;; Prompt definitions
   #:define-prompt
   #:*prompt-registry*))
