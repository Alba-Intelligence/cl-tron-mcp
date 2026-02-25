;;;; src/transport/http-hunchentoot.lisp - HTTP transport via Hunchentoot
;;;; Replaces the usocket-based HTTP transport so responses are delivered
;;;; correctly. Requires :hunchentoot in the system dependencies.

(in-package :cl-tron-mcp/transport)

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:hunchentoot :flexi-streams) :silent t))

(defvar *http-acceptor* nil
  "Hunchentoot acceptor for HTTP transport.")

(defun handle-rpc-body (body)
  "Parse body as JSON-RPC, call protocol handler. Returns (values json-body-string status-code)."
  (let ((message (ignore-errors (jonathan:parse body))))
    (if (not message)
        (values "{}" 400)
        (handler-case
            (let ((response (cl-tron-mcp/protocol:handle-message message)))
              (values (cond ((and response (stringp response)) response)
                           (response (jonathan:to-json response))
                           (t "{}"))
                      200))
          (error (e)
            (values (jonathan:to-json
                     (list :|jsonrpc| "2.0" :|id| nil :|error|
                           (list :|code| -32603 :|message| (princ-to-string e))))
                    200))))))

;;; Hunchentoot handlers return the response body; we set headers and status.
;;; For full control we use hunchentoot:reply and hunchentoot:content-type*.

(hunchentoot:define-easy-handler (mcp-health :uri "/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{\"status\":\"ok\"}")

(hunchentoot:define-easy-handler (mcp-root :uri "/") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{\"service\":\"cl-tron-mcp\",\"status\":\"running\"}")

(hunchentoot:define-easy-handler (mcp-lisply-ping :uri "/lisply/ping-lisp") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{\"status\":\"pong\"}")

(hunchentoot:define-easy-handler (mcp-lisply-tools-list :uri "/lisply/tools/list") ()
  (setf (hunchentoot:content-type*) "application/json")
  (jonathan:to-json (list :tools (cl-tron-mcp/tools:list-tool-descriptors))))

(hunchentoot:define-easy-handler (mcp-rpc :uri "/rpc" :default-request-type :post) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (if (or (null body) (zerop (length body)))
        (progn (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+) "")
        (multiple-value-bind (body-json status)
            (handle-rpc-body body)
          (setf (hunchentoot:return-code*) (case status (200 hunchentoot:+http-ok+) (t hunchentoot:+http-bad-request+)))
          (or body-json "{}")))))

(hunchentoot:define-easy-handler (mcp-lisply-eval :uri "/lisply/lisp-eval" :default-request-type :post) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (if (or (null body) (zerop (length body)))
        (progn (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+) "")
        (let ((out (lisp-eval-handler body)))
          ;; lisp-eval-handler returns full HTTP response string; extract body after \r\n\r\n
          (let ((start (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) out)))
            (if start (subseq out (+ start 4)) out))))))

(defun mcp-dispatch-table ()
  "Dispatch table for MCP HTTP routes (Hunchentoot). /mcp and /rpc both accept POST JSON-RPC (Kilocode streamable-http uses url .../mcp)."
  (list (hunchentoot:create-regex-dispatcher "^/health$" #'mcp-health)
        (hunchentoot:create-regex-dispatcher "^/lisply/ping-lisp$" #'mcp-lisply-ping)
        (hunchentoot:create-regex-dispatcher "^/lisply/tools/list$" #'mcp-lisply-tools-list)
        (hunchentoot:create-regex-dispatcher "^/lisply/lisp-eval$" #'mcp-lisply-eval)
        (hunchentoot:create-regex-dispatcher "^/mcp$" #'mcp-rpc)
        (hunchentoot:create-regex-dispatcher "^/rpc$" #'mcp-rpc)
        (hunchentoot:create-regex-dispatcher "^/$" #'mcp-root)))

(defun start-http-transport (&key (port 4006) (block t))
  "Start HTTP transport using Hunchentoot. When BLOCK is true (default), block until stop-http-transport.
   When BLOCK is false, start the server and return (for combined stdio+http)."
  (when *http-acceptor*
    (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server already running on port ~d" port))
    (return-from start-http-transport))
  (setf *http-acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :address "127.0.0.1"
                       :document-root nil
                       :access-log-destination nil
                       :message-log-destination nil))
  (setf hunchentoot:*dispatch-table* (mcp-dispatch-table))
  (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server starting on http://127.0.0.1:~d (Hunchentoot)" port))
  (hunchentoot:start *http-acceptor*)
  (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server listening on http://127.0.0.1:~d - POST /rpc for JSON-RPC" port))
  (setf *http-running* t)
  (if block
      (progn
        (loop while *http-running* do (bt:thread-yield) (sleep 1))
        (hunchentoot:stop *http-acceptor*)
        (setf *http-acceptor* nil)
        (setf *http-running* nil))
      (bt:make-thread
       (lambda ()
         (loop while *http-running* do (bt:thread-yield) (sleep 1))
         (when *http-acceptor*
           (ignore-errors (hunchentoot:stop *http-acceptor*))
           (setf *http-acceptor* nil))
         (setf *http-running* nil))
       :name "http-watchdog")))

(defun stop-http-transport ()
  "Stop the Hunchentoot HTTP server."
  (setf *http-running* nil)
  (when *http-acceptor*
    (ignore-errors (hunchentoot:stop *http-acceptor*))
    (setf *http-acceptor* nil))
  (cl-tron-mcp/logging:log-info "[MCP] HTTP server stopped"))
