;;;; src/protocol/messages.lisp

(in-package :cl-tron-mcp/protocol)

(defun parse-message (json-string)
  "Parse JSON-RPC 2.0 message."
  (jonathan:parse json-string))

(defun make-response (id result)
  "Create JSON-RPC 2.0 response."
  (jonathan:to-json (list :jsonrpc "2.0" :id id :result result)))

(defun make-error-response (id code message &optional data)
  "Create JSON-RPC 2.0 error response."
  (jonathan:to-json (list :jsonrpc "2.0"
                          :id id
                          :error (list :code code
                                       :message message
                                       :data data))))

(defun make-notification (method params)
  "Create JSON-RPC 2.0 notification."
  (jonathan:to-json (list :jsonrpc "2.0" :method method :params params)))
