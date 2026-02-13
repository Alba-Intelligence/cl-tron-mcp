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
  (format nil "HTTP/1.1 200 OK~C~CContent-Type: ~a~C~CContent-Length: ~d~C~C~a"
          #\Return #\Newline content-type #\Return #\Newline (length content) #\Return #\Newline content))

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

(defun read-http-request (stream)
  (let ((request-line (read-line stream nil nil)))
    (when request-line
      (let ((headers (make-hash-table :test 'equal))
            (content-length 0))
        (loop for line = (read-line stream nil nil)
              until (or (null line) (string= line ""))
              do
              (let ((colon (position #\: line)))
                (when colon
                  (let ((name (subseq line 0 colon))
                        (value (string-trim " " (subseq line (1+ colon)))))
                    (setf (gethash name headers) value)
                    (when (string= (string-downcase name) "content-length")
                      (setf content-length (parse-integer value :junk-allowed t)))))))
        (let ((body (when (> content-length 0)
                      (let ((buf (make-string content-length)))
                        (read-sequence buf stream)
                        buf))))
          (values request-line headers body))))))

(defun handle-http-client (client-stream)
  (multiple-value-bind (request-line headers body) (read-http-request client-stream)
    (when request-line
      (let* ((parts (split-string request-line #\Space))
             (method (car parts))
             (path (cadr parts)))
        (format t "[MCP-HTTP] ~a ~a~%" method path)
        (force-output)
        (cond
          ;; lisply-mcp endpoints
          ((and (string= method "POST") (search "/lisply/lisp-eval" path) body)
           (format client-stream "~a" (lisp-eval-handler body))
           (force-output client-stream))
          ((and (string= method "GET") (string= path "/lisply/ping-lisp"))
           (format client-stream "~a" (http-ok "{\"status\":\"pong\"}"))
           (force-output client-stream))
          ((and (string= method "GET") (string= path "/lisply/tools/list"))
           (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
             (format client-stream "~a" (http-ok (jonathan:to-json (list :tools tools))))
             (force-output client-stream)))
          ;; Original endpoints
          ((and (string= method "GET") (string= path "/"))
           (let ((tools (cl-tron-mcp/tools:list-tool-descriptors)))
             (format client-stream "~a" (http-ok (jonathan:to-json tools) "application/json"))
             (force-output client-stream)))
          ((and (string= method "POST") (string= path "/rpc"))
           (when body
             (let ((message (ignore-errors (jonathan:parse body))))
               (if message
                   (let ((response (cl-tron-mcp/protocol:handle-message message)))
                     (format client-stream "~a"
                             (http-ok (jonathan:to-json response)
                                      "application/json"))
                     (force-output client-stream))
                   (format client-stream "~a" (http-bad-request))))))
          ((string= path "/health")
           (format client-stream "~a"
                   (http-ok "{\"status\":\"ok\"}" "application/json"))
           (force-output client-stream))
          ((string= path "/")
           (format client-stream "~a"
                   (http-ok "{\"service\":\"cl-tron-mcp\",\"status\":\"running\"}" "application/json"))
           (force-output client-stream))
          (t
           (format client-stream "~a" (http-not-found))
           (force-output client-stream))))))
  (ignore-errors (close client-stream)))

(defun http-server-loop (port)
  (setf *http-running* t)
  (format t "[MCP] HTTP server starting on http://127.0.0.1:~d~%" port)
  (handler-case
      (let ((server-socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
        (format t "[MCP] HTTP server listening on port ~d~%" port)
        (format t "[MCP] Endpoints:~%")
        (format t "[MCP]   GET  /               - List available tools~%")
        (format t "[MCP]   POST /rpc             - Send MCP JSON-RPC message~%")
        (format t "[MCP]   GET  /health          - Health check~%")
        (format t "[MCP]   POST /lisply/lisp-eval - Evaluate Lisp code (lisply-mcp protocol)~%")
        (format t "[MCP]   GET  /lisply/ping-lisp - Ping endpoint (lisply-mcp protocol)~%")
        (format t "[MCP]   GET  /lisply/tools/list - List tools (lisply-mcp protocol)~%")
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
  (when *http-server*
    (format t "[MCP] HTTP server already running on port ~d~%" port)
    (return-from start-http-transport))
  (setf *http-thread*
        (bt:make-thread (lambda () (http-server-loop port))
                        :name "http-server")))

(defun stop-http-transport ()
  (setf *http-running* nil)
  (when *http-server*
    (ignore-errors (usocket:socket-close *http-server*))
    (setf *http-server* nil))
  (format t "[MCP] HTTP server stopped~%"))

(defun send-message-via-http (message)
  (declare (ignore message))
  nil)
