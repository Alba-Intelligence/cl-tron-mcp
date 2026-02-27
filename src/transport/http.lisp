;;;; src/transport/http.lisp
;;;;
;;;; HTTP transport layer for MCP protocol.
;;;;
;;;; This file provides a simple HTTP server for testing and development.
;;;; For production use, use the Hunchentoot-based transport.

(in-package :cl-tron-mcp/transport)

;;; ============================================================
;;; HTTP Response Helpers
;;; ============================================================

(defun http-ok (content &optional (content-type "application/json"))
  "Create HTTP 200 OK response with CONTENT."
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

;;; ============================================================
;;; Lisp Eval Handler
;;; ============================================================

(defun lisp-eval-handler (body-str)
  "Handle HTTP request to evaluate Lisp code.
Expects JSON with 'code' and optional 'package' fields."
  (let ((parsed (ignore-errors (jonathan:parse body-str))))
    (when (null parsed)
      (return-from lisp-eval-handler
        (http-error-response "Invalid JSON in request body")))
    (let ((code-val (getf parsed :|code|))
          (pkg-val (getf parsed :|package| "CL-USER")))
      (when (null code-val)
        (return-from lisp-eval-handler
          (http-error-response "Missing 'code' field")))
      (let ((pkg-obj (find-package (string-upcase pkg-val))))
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
                                                   :|stdout| stdout-str)))))))))))

;;; ============================================================
;;; HTTP Request Parsing
;;; ============================================================

(defun blank-line-p (line)
  (zerop (length (string-trim '(#\Return #\Newline #\Space) (or line "")))))

(defun read-http-request (stream)
  "Read HTTP request from STREAM.
Returns (values request-line headers body)."
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

;;; ============================================================
;;; Simple HTTP Server
;;; ============================================================

(defun start-simple-http-server (&optional (port 4006))
  "Start a simple HTTP server on PORT for testing.
This is a minimal implementation for development only."
  (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
    (format t "Simple HTTP server listening on port ~d~%" port)
    (unwind-protect
        (loop
          (handler-case
              (let ((client-socket (usocket:socket-accept socket)))
                (unwind-protect
                    (let ((stream (usocket:socket-stream client-socket)))
                      (multiple-value-bind (request-line headers body)
                          (read-http-request stream)
                        (declare (ignore headers))
                        (when request-line
                          (let ((method (subseq request-line 0 (position #\Space request-line)))
                                (path (subseq request-line (1+ (position #\Space request-line))
                                               (position #\Space request-line :from-end t))))
                            (cond
                             ((and (string= method "POST")
                                   (string= path "/eval"))
                              (write-string (lisp-eval-handler body) stream))
                             (t (write-string (http-not-found) stream)))
                            (finish-output stream)))))
                  (usocket:socket-close client-socket)))
            (usocket:socket-error (e)
              (format t "Socket error: ~a~%" e))
            (error (e)
              (format t "Error handling request: ~a~%" e))))
      (usocket:socket-close socket))))

(provide :cl-tron-mcp/transport-http)
