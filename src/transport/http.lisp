;;;; src/transport/http.lisp - Legacy HTTP helpers and usocket loop for CL-TRON-MCP
;;;; The active HTTP transport is Hunchentoot (src/transport/http-hunchentoot.lisp),
;;;; loaded after this file; it defines start-http-transport and stop-http-transport.
;;;; This file provides http-ok, lisp-eval-handler, parse-http-request, and the
;;;; usocket-based server loop (used only if http-hunchentoot is not loaded).

(in-package :cl-tron-mcp/transport)

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t))

(defvar *http-server* nil)
(defvar *http-thread* nil)
(defvar *http-running* nil)
(defvar *http-clients* nil)

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun normalize-crlf (string)
  (remove #\Return string))

(defun parse-http-request (request)
  (let* ((request (normalize-crlf request))
         (lines (split-string request #\Newline))
         (request-line (car lines))
         (parts (split-string request-line #\Space))
         (method (car parts))
         (path (cadr parts))
         (headers nil)
         (body nil)
         (in-body nil))
    (dolist (line (cdr lines))
      (if in-body
          (setf body (concatenate 'string body (string #\Newline) line))
          (if (string= line "")
              (setf in-body t)
              (let ((colon (position #\: line)))
                (when colon
                  (push (cons (subseq line 0 colon)
                              (string-trim " " (subseq line (1+ colon))))
                        headers))))))
    (list :method method :path path :headers (reverse headers) :body body)))

(defun http-ok (content &optional (content-type "application/json"))
  (let ((body (cond ((and (stringp content) (plusp (length content))) content)
                    ((stringp content) "{}")
                    (t (princ-to-string content)))))
    (when (zerop (length body)) (setq body "{}"))
    (format nil "HTTP/1.1 200 OK~C~CContent-Type: ~a~C~CContent-Length: ~d~C~C~a"
            #\Return #\Newline content-type #\Return #\Newline (length body) #\Return #\Newline body)))

(defun http-error-response (message)
  (http-ok (format nil "{\"success\": false, \"error\": \"~a\"}" message)))

(defun http-not-found ()
  (format nil "HTTP/1.1 404 Not Found~C~CContent-Length: 0~C~C" #\Return #\Newline #\Return #\Newline))

(defun http-bad-request ()
  (format nil "HTTP/1.1 400 Bad Request~C~CContent-Length: 0~C~C" #\Return #\Newline #\Return #\Newline))

(defun lisp-eval-handler (body-str)
  (let ((parsed (ignore-errors (jonathan:parse body-str))))
    (when (null parsed)
      (return-from lisp-eval-handler
        (http-error-response "Invalid JSON in request body")))
    (let ((code-val (getf parsed :|code|))
          (pkg-val (getf parsed :|package| "CL-USER"))
          (pkg-obj (find-package (string-upcase pkg-val))))
      (when (null code-val)
        (return-from lisp-eval-handler
          (http-error-response "Missing 'code' field")))
      (when (null pkg-obj)
        (return-from lisp-eval-handler
          (http-error-response (format nil "Package ~a not found" pkg-val))))
      (let ((*package* pkg-obj)
            (*print-pretty* nil)
            (output (make-string-output-stream)))
        (let ((eval-result
                (handler-case
                    (eval (read-from-string code-val))
                  (reader-error (e) (cons :err (princ-to-string e)))
                  (error (e) (cons :err (princ-to-string e))))))
          (let ((stdout-str (get-output-stream-string output)))
            (if (and (consp eval-result) (eq (car eval-result) :err))
                (http-error-response (cdr eval-result))
                (http-ok (jonathan:to-json (list :|success| t
                                                 :|result| (format nil "~a" eval-result)
                                                 :|stdout| stdout-str))))))))))

(defun blank-line-p (line)
  (zerop (length (string-trim '(#\Return #\Newline #\Space) (or line "")))))

(defun read-http-request (stream)
  (let ((request-line (read-line stream nil nil)))
    (when request-line
      (setq request-line (string-trim '(#\Return #\Newline) request-line))
      (let ((headers (make-hash-table :test 'equal))
            (content-length 0))
        (loop for line = (read-line stream nil nil)
              until (or (null line) (blank-line-p line))
              do
                 (setq line (string-trim '(#\Return #\Newline) line))
                 (let ((colon (position #\: line)))
                   (when colon
                     (let ((name (string-trim " " (subseq line 0 colon)))
                           (value (string-trim " " (subseq line (1+ colon)))))
                       (setf (gethash name headers) value)
                       (when (string= (string-downcase name) "content-length")
                         (setf content-length (parse-integer value :junk-allowed t)))))))
        (let ((body (when (> content-length 0)
                      (let ((buf (make-string content-length)))
                        (read-sequence buf stream)
                        buf))))
          (values request-line headers body))))))


(defun header-body-separator ()
  (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline))

(defun send-response (stream response-string)
  "Write HTTP response (headers then body) and flush. Does not close stream;
   caller should close the socket with usocket:socket-close for proper TCP shutdown."
  (handler-case
      (let* ((sep-str (header-body-separator))
             (sep (search sep-str response-string)))
        (if sep
            (let ((headers-end (+ sep (length sep-str))))
              (write-string (subseq response-string 0 headers-end) stream)
              (force-output stream)
              (write-string (subseq response-string headers-end) stream)
              (force-output stream)
              (finish-output stream))
            (progn
              (write-string response-string stream)
              (force-output stream)
              (finish-output stream))))
    (error (e)
      (cl-tron-mcp/logging:log-error (format nil "[MCP-HTTP] send-response error: ~a" e)))))

(defun handle-rpc (client-stream body)
  "Handle POST /rpc: parse body as JSON-RPC, call protocol handler, send response."
  (let ((message (ignore-errors (jonathan:parse body))))
    (if (not message)
        (send-response client-stream (http-bad-request))
        (handler-case
            (let ((response (cl-tron-mcp/protocol:handle-message message)))
              (let ((body-json (cond ((and response (stringp response)) response)
                                     (response (jonathan:to-json response))
                                     (t "{}"))))
                (send-response client-stream (http-ok body-json "application/json"))))
          (error (e)
            (send-response client-stream
                           (http-ok (jonathan:to-json
                                     (list :|jsonrpc| "2.0" :|id| nil :|error|
                                           (list :|code| -32603 :|message| (princ-to-string e))))
                                    "application/json")))))))

(defun handle-http-client (client-socket)
  "Handle one HTTP request; stream is socket-stream of client-socket. Close client-socket when done."
  (let ((client-stream (usocket:socket-stream client-socket)))
    (multiple-value-bind (request-line headers body) (read-http-request client-stream)
      (when request-line
        (let* ((parts (split-string request-line #\Space))
               (method (car parts))
               (path (cadr parts)))
          (cl-tron-mcp/logging:log-info (format nil "[MCP-HTTP] ~a ~a" method path))
          (cond
            ;; Path-only: no method branch (health, root service status)
            ((string= path "/health")
             (send-response client-stream (http-ok "{\"status\":\"ok\"}" "application/json")))
            ;; GET
            ((string= method "GET")
             (cond
               ((string= path "/lisply/ping-lisp")
                (send-response client-stream (http-ok "{\"status\":\"pong\"}" "application/json")))
               ((string= path "/lisply/tools/list")
                (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
                  (send-response client-stream
                                 (http-ok (jonathan:to-json (list :tools tools)) "application/json"))))
               ((string= path "/")
                (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
                  (send-response client-stream
                                 (http-ok (jonathan:to-json tools) "application/json"))))
               (t
                (send-response client-stream (http-not-found)))))
            ;; POST (body required; check immediately)
            ((string= method "POST")
             (if (not body)
                 (send-response client-stream (http-bad-request))
                 (cond
                   ((search "/lisply/lisp-eval" path)
                    (send-response client-stream (lisp-eval-handler body)))
                   ((or (string= path "/rpc") (string= path "/mcp"))
                    (handle-rpc client-stream body))
                   (t
                    (send-response client-stream (http-not-found))))))
            ;; Default: root path = service status, else 404
            ((string= path "/")
             (send-response client-stream
                            (http-ok "{\"service\":\"cl-tron-mcp\",\"status\":\"running\"}"
                                     "application/json")))
            (t
             (send-response client-stream (http-not-found))))))
      (ignore-errors (usocket:socket-close client-socket)))))


(defun http-dbg (msg)
  (ignore-errors
   (let ((root (when (boundp 'cl-user::*http-project-root*)
                 (symbol-value 'cl-user::*http-project-root*))))
     (let ((base (or root *default-pathname-defaults*)))
       (ensure-directories-exist (merge-pathnames "reports/" base))
       (with-open-file (f (merge-pathnames "reports/http-startup.log" base)
                          :direction :output :if-exists :append :if-does-not-exist :create)
         (write-line (format nil "~a ~a" (get-universal-time) msg) f))))))

(defun http-server-loop (port)
  (http-dbg "http-server-loop: entered")
  (handler-case
      (let ((port (etypecase port
                    (integer port)
                    (string (parse-integer port)))))
        (setf *http-running* t)
        (http-dbg (format nil "http-server-loop: port=~a calling socket-listen" port))
        (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server starting on http://127.0.0.1:~d" port))
        (handler-case
            (let ((server-socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
              (http-dbg "http-server-loop: socket-listen done, listening")
              (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server listening on port ~d" port))
              (format *error-output* "~&[MCP] HTTP server listening on http://127.0.0.1:~d - POST /rpc for JSON-RPC~%" port)
              (force-output *error-output*)
              (cl-tron-mcp/logging:log-info "[MCP] Endpoints:")
              (cl-tron-mcp/logging:log-info "[MCP]   GET  /               - List available tools")
              (cl-tron-mcp/logging:log-info "[MCP]   POST /rpc             - Send MCP JSON-RPC message")
              (cl-tron-mcp/logging:log-info "[MCP]   GET  /health          - Health check")
              (cl-tron-mcp/logging:log-info "[MCP]   POST /lisply/lisp-eval - Evaluate Lisp code (lisply-mcp protocol)")
              (cl-tron-mcp/logging:log-info "[MCP]   GET  /lisply/ping-lisp - Ping endpoint (lisply-mcp protocol)")
              (cl-tron-mcp/logging:log-info "[MCP]   GET  /lisply/tools/list - List tools (lisply-mcp protocol)")
              (setf *http-server* server-socket)
              (loop
                (when (not *http-running*)
                  (return))
                (let ((client-socket (handler-case (usocket:socket-accept server-socket)
                                       (error () nil))))
                  (when client-socket
                    (bt:make-thread
                     (lambda () (handle-http-client client-socket))
                     :name "http-client")))))
          (serious-condition (e)
            (http-dbg (format nil "http-server-loop: socket-listen/loop error: ~a" e))
            (setf *http-running* nil)
            (cl-tron-mcp/logging:log-error (format nil "[MCP] HTTP server error: ~a" e))
            (format *error-output* "~&[MCP] HTTP server failed: ~a~%" e)
            (force-output *error-output*)))
        (setf *http-running* nil))
    (serious-condition (e)
      (setf *http-running* nil)
      (cl-tron-mcp/logging:log-error (format nil "[MCP] HTTP server serious error: ~a" e))
      (format *error-output* "~&[MCP] HTTP server serious error: ~a~%" e)
      (force-output *error-output*)
      (signal e))))

(defun start-http-transport (&key (port 4005) (block t))
  "Start HTTP transport. When BLOCK is true (default), block until the server stops.
   When BLOCK is false, start the server in a background thread and return (for combined stdio+http)."
  (http-dbg "start-http-transport: entered")
  (when *http-server*
    (http-dbg "start-http-transport: already running, returning")
    (cl-tron-mcp/logging:log-info (format nil "[MCP] HTTP server already running on port ~d" port))
    (return-from start-http-transport))
  (http-dbg "start-http-transport: creating thread")
  (setf *http-thread*
        (bt:make-thread (lambda () (http-server-loop port))
                        :name "http-server"))
  (http-dbg (format nil "start-http-transport: thread=~a block=~a" (and *http-thread* t) block))
  (when (and block *http-thread*)
    (handler-case (bt:join-thread *http-thread*)
      (serious-condition (e)
        (cl-tron-mcp/logging:log-error (format nil "[MCP] HTTP server thread exited: ~a" e))
        (format *error-output* "~&[MCP] HTTP server thread exited: ~a~%" e)
        (force-output *error-output*)))))

(defun stop-http-transport ()
  (setf *http-running* nil)
  (when *http-server*
    (ignore-errors (usocket:socket-close *http-server*))
    (setf *http-server* nil))
  (cl-tron-mcp/logging:log-info "[MCP] HTTP server stopped"))

(defun send-message-via-http (message)
  (declare (ignore message))
  nil)
