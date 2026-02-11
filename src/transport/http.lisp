;;;; src/transport/http.lisp - HTTP Transport for CL-TRON-MCP

(in-package :cl-tron-mcp/transport)

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t))

(defvar *http-server* nil)
(defvar *http-thread* nil)
(defvar *http-running* nil)
(defvar *http-clients* nil)

(defun split-string (string delimiter)
  "Split string by delimiter."
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun parse-http-request (request)
  "Parse HTTP request and return method, path, headers, and body."
  (let* ((lines (split-string request #\Newline))
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

(defun http-ok (content content-type)
  "Create HTTP 200 response."
  (format nil "HTTP/1.1 200 OK~C~CContent-Type: ~a~C~CContent-Length: ~d~C~C~a"
          #\Return #\Newline content-type #\Return #\Newline (length content) #\Return #\Newline content))

(defun http-not-found ()
  "Create HTTP 404 response."
  (format nil "HTTP/1.1 404 Not Found~C~CContent-Length: 0~C~C" #\Return #\Newline #\Return #\Newline))

(defun http-bad-request ()
  "Create HTTP 400 response."
  (format nil "HTTP/1.1 400 Bad Request~C~CContent-Length: 0~C~C" #\Return #\Newline #\Return #\Newline))

(defun handle-http-client (client-stream)
  "Handle HTTP client connection."
  (let ((request (read-line client-stream nil nil)))
    (when request
      (let* ((parsed (parse-http-request request))
             (method (getf parsed :method))
             (path (getf parsed :path))
             (body (getf parsed :body)))
        (format t "[MCP-HTTP] ~a ~a~%" method path)
        (cond
          ((and (string= method "GET") (string= path "/"))
           (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
             (format client-stream "~a" (http-ok (jonathan:to-json tools) "application/json"))))
          ((and (string= method "POST") (string= path "/rpc"))
           (when body
             (let ((message (ignore-errors (jonathan:parse body))))
               (if message
                   (let ((response (cl-tron-mcp/protocol:handle-message message)))
                     (format client-stream "~a"
                             (http-ok (jonathan:to-json response)
                                      "application/json")))
                   (format client-stream "~a" (http-bad-request))))))
          ((string= path "/health")
           (format client-stream "~a"
                   (http-ok "{\"status\":\"ok\"}" "application/json")))
          ((string= path "/")
           (format client-stream "~a"
                   (http-ok "{\"service\":\"cl-tron-mcp\",\"status\":\"running\"}" "application/json")))
          (t
           (format client-stream "~a" (http-not-found))))))
    (ignore-errors (close client-stream))))

(defun http-server-loop (port)
  "Main server loop for HTTP connections."
  (setf *http-running* t)
  (format t "[MCP] HTTP server starting on http://127.0.0.1:~d~%" port)
  (handler-case
      (let ((server-socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
        (format t "[MCP] HTTP server listening on port ~d~%" port)
        (format t "[MCP] Endpoints:~%")
        (format t "[MCP]   GET  /          - List available tools~%")
        (format t "[MCP]   POST /rpc        - Send MCP JSON-RPC message~%")
        (format t "[MCP]   GET  /health     - Health check~%")
        (setf *http-server* server-socket)
        (loop
          (when (not *http-running*)
            (return))
          (let ((client-socket (handler-case (usocket:socket-accept server-socket)
                                (error () nil))))
            (when client-socket
              (bt:make-thread
               (lambda () (handle-http-client (usocket:socket-stream client-socket)))
               :name "http-client")))))
    (error (e)
      (format t "[MCP] HTTP server error: ~a~%" e)))
  (setf *http-running* nil))

(defun start-http-transport (&key (port 8080))
  "Start HTTP transport on specified port."
  (when *http-server*
    (format t "[MCP] HTTP server already running on port ~d~%" port)
    (return-from start-http-transport))
  (setf *http-thread*
        (bt:make-thread (lambda () (http-server-loop port))
                        :name "http-server")))

(defun stop-http-transport ()
  "Stop HTTP transport."
  (setf *http-running* nil)
  (when *http-server*
    (ignore-errors (usocket:socket-close *http-server*))
    (setf *http-server* nil))
  (format t "[MCP] HTTP server stopped~%"))

(defun send-message-via-http (message)
  "Send message via HTTP (not applicable for server mode)."
  (declare (ignore message))
  nil)
