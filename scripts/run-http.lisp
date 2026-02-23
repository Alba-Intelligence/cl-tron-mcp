;;;; scripts/run-http.lisp
;;;; Loaded by start-mcp.sh --http to avoid a long final --eval (which can be dropped by the shell).
;;;; Reads port from http-port.txt in the project root (parent of this script's directory).

;; Project root = parent of this script's directory. Used so transport can write to same startup log.
(defvar *http-project-root* nil)

(defun http-dbg (msg)
  (ignore-errors
    (let ((root (or *http-project-root* *default-pathname-defaults*)))
      (ensure-directories-exist (merge-pathnames "reports/" root))
      (let ((log-path (merge-pathnames "reports/http-startup.log" root)))
        (with-open-file (f log-path :direction :output :if-exists :append :if-does-not-exist :create)
          (write-line (format nil "~a ~a" (get-universal-time) msg) f))))))

(let ((project-root (if *load-pathname*
                       (make-pathname :directory (append (pathname-directory (truename *load-pathname*))
                                                         (list :up))
                                      :defaults (truename *load-pathname*))
                       *default-pathname-defaults*)))
  (setq *http-project-root* (truename (merge-pathnames "" project-root)))
  (let ((port-file (merge-pathnames "http-port.txt" *http-project-root*)))
    (http-dbg "run-http: entered")
    (unless (probe-file port-file)
      (http-dbg (format nil "run-http: missing port file ~a" port-file))
      (format *error-output* "~&[MCP] Missing ~a (script should write PORT here).~%" port-file)
      (force-output *error-output*)
      (#+sbcl sb-ext:quit #+ecl ext:quit #-(or sbcl ecl) cl-user::quit 1))
    (let ((port (parse-integer (with-open-file (f port-file) (read-line f)))))
      (http-dbg (format nil "run-http: port=~a calling start-server" port))
      (handler-case
          (progn
            (cl-tron-mcp/core:start-server :transport :http :port port)
            (http-dbg "run-http: start-server returned, quitting 0")
            (#+sbcl sb-ext:quit #+ecl ext:quit #-(or sbcl ecl) cl-user::quit 0))
        (condition (c)
          (http-dbg (format nil "run-http: condition ~a" c))
          (format *error-output* "~&[MCP] Error: ~a~%" c)
          (force-output *error-output*)
          (#+sbcl sb-ext:quit #+ecl ext:quit #-(or sbcl ecl) cl-user::quit 1))))))
