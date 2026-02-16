;;;; src/resources/package.lisp
;;;;
;;;; MCP Resources module for cl-tron-mcp
;;;;
;;;; This module implements the MCP Resources protocol, allowing AI agents
;;;; to discover and read documentation files exposed by the server.
;;;;
;;;; MCP Resources Protocol:
;;;; ======================
;;;; Resources in MCP are application-driven context that servers expose to clients.
;;;; They are identified by URIs and can contain text or binary data.
;;;;
;;;; Protocol Methods:
;;;;   - resources/list   : Returns list of available resources
;;;;   - resources/read   : Returns contents of a specific resource
;;;;
;;;; Security:
;;;;   - Only whitelisted files are exposed
;;;;   - Path traversal is prevented
;;;;   - No sensitive files (secrets, .env, etc.) are exposed
;;;;
;;;; See: https://modelcontextprotocol.io/docs/concepts/resources

(defpackage :cl-tron-mcp/resources
  (:use :cl)
  (:nicknames :tron/resources)
  (:export
   ;; Resource listing
   #:list-resources
   #:resource-descriptor
   
   ;; Resource reading
   #:read-resource
   #:resource-not-found-error
   
   ;; Whitelist management
   #:*resource-whitelist*
   #:add-resource-to-whitelist
   #:clear-resource-whitelist
   
   ;; MCP handlers
   #:handle-resources-list
   #:handle-resources-read))
