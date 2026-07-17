;;;; src/protocol/messages.lisp

(in-package :cl-tron-mcp/protocol)

(defun parse-message (json-string)
  "Parse JSON-RPC 2.0 message."
  (cl-tron-mcp/json-compat:parse json-string))

(defun make-response (id result)
  "Create JSON-RPC 2.0 response."
  (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0" :|id| id :|result| result)))

(defun make-error-response (id code message &optional data)
  "Create JSON-RPC 2.0 error response."
  (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
                          :|id| id
                          :|error| (list :|code| code
                                         :|message| message
                                         :|data| data))))

(defun make-notification (method params)
  "Create JSON-RPC 2.0 notification."
  (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0" :|method| method :|params| params)))
