;;;; src/transport/http-hunchentoot.lisp - HTTP transport via Hunchentoot
;;;; Replaces the usocket-based HTTP transport so responses are delivered
;;;; correctly. Requires :hunchentoot in the system dependencies.

(in-package :cl-tron-mcp/transport)

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:hunchentoot :flexi-streams) :silent t))

;;; HTTP Transport Configuration
(defvar *max-concurrent-connections* 100
  "Maximum number of concurrent HTTP connections.")

(defvar *http-request-timeout* 30
  "Default timeout for HTTP requests in seconds.")

(defvar *rate-limit-enabled* t
  "Enable rate limiting for HTTP requests.")

(defvar *rate-limit-requests-per-minute* 60
  "Maximum requests per minute per IP address.")

(defvar *max-request-size* (* 10 1024 1024)
  "Maximum size of HTTP request body in bytes.")

(defvar *http-connection-timeout* 10
  "Connection timeout in seconds.")

;;; Rate limiting state
(defvar *request-counts* (make-hash-table :test 'equal)
  "Hash table tracking request counts per IP address.")

(defvar *request-count-lock* (bt:make-lock "request-counts")
  "Lock for synchronizing access to *request-counts*.")

(defvar *http-acceptor* nil
  "Hunchentoot acceptor for HTTP transport.")

(defvar *http-running* nil
  "Flag indicating whether the HTTP server is running.")

(defun check-rate-limit (ip-address)
  "Check if IP-ADDRESS has exceeded rate limit.
Returns T if allowed, NIL if rate limited."
  (unless *rate-limit-enabled*
    (return-from check-rate-limit t))

  (bt:with-lock-held (*request-count-lock*)
    (let* ((now (get-universal-time))
           (entry (gethash ip-address *request-counts*))
           (count (if entry (getf entry :count) 0))
           (last-reset (if entry (getf entry :last-reset) 0)))
      (when (> (- now last-reset) 60)
        (setf count 0
              last-reset now))
      (if (>= count *rate-limit-requests-per-minute*)
          (progn
            (cl-tron-mcp/logging:log-warn
             (format nil "Rate limit exceeded for IP: ~a" ip-address))
            nil)
          (progn
            (setf (gethash ip-address *request-counts*)
                  (list :count (1+ count) :last-reset last-reset))
            t)))))

(defun handle-rpc-body (body)
  "Parse body as JSON-RPC, call protocol handler with timeout and size limits.
Returns (values json-body-string status-code)."
  (when (> (length body) *max-request-size*)
    (cl-tron-mcp/logging:log-warn
     (format nil "Request too large: ~d bytes (max: ~d)"
             (length body) *max-request-size*))
    (return-from handle-rpc-body
      (values (jonathan:to-json
               (list :|jsonrpc| "2.0" :|id| nil :|error|
                     (list :|code| -32602 :|message| "Request too large")))
              413)))

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
            (cl-tron-mcp/logging:log-error (format nil "Error handling RPC: ~a" e))
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

  (let ((ip-address (hunchentoot:remote-addr*)))
    (unless (check-rate-limit ip-address)
      (setf (hunchentoot:return-code*) 429)
      (return-from mcp-rpc
        (jonathan:to-json
         (list :|error| (list :|message| "Rate limit exceeded"))))))

  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (if (or (null body) (zerop (length body)))
        (progn (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+) "")
        (multiple-value-bind (body-json status)
            (handle-rpc-body body)
          (setf (hunchentoot:return-code*) (case status
                                              (200 hunchentoot:+http-ok+)
                                              (413 hunchentoot:+http-request-entity-too-large+)
                                              (t hunchentoot:+http-bad-request+)))
          (or body-json "{}")))))

(hunchentoot:define-easy-handler (mcp-lisply-eval :uri "/lisply/lisp-eval" :default-request-type :post) ()
  (setf (hunchentoot:content-type*) "application/json")

  (let ((ip-address (hunchentoot:remote-addr*)))
    (unless (check-rate-limit ip-address)
      (setf (hunchentoot:return-code*) 429)
      (return-from mcp-lisply-eval
        (jonathan:to-json
         (list :|error| (list :|message| "Rate limit exceeded"))))))

  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (if (or (null body) (zerop (length body)))
        (progn (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+) "")
        (let ((out (lisp-eval-handler body)))
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
  "Start HTTP transport using Hunchentoot with proper limits and timeouts. When BLOCK is true (default), block until stop-http-transport.
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
                       :message-log-destination nil
                       :request-class 'hunchentoot:request
                       :reply-class 'hunchentoot:reply
                       :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
                       :input-chunking-p t
                       :read-timeout *http-request-timeout*
                       :write-timeout *http-request-timeout*
                       :persistent-connections-p t))

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
