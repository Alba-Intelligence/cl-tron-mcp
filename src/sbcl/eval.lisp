;;;; src/sbcl/eval.lisp

(in-package :cl-tron-mcp/sbcl)

(defvar *eval-timeout* 30)

(defun safe-eval (code &key (package :cl-user) (timeout *eval-timeout*) (safe-read t))
  "Evaluate Lisp code safely with timeout and error handling."
  (let ((*package* (or (find-package package)
                        (error "Package ~a not found" package))))
    (handler-case
        (let ((form (read-from-string code)))
          (when safe-read
            (setf *read-eval* nil))
          (eval form))
      (reader-error (e)
        (list :error t
              :type "READER-ERROR"
              :message (princ-to-string e)))
      (error (e)
        (list :error t
              :type (prin1-to-string (type-of e))
              :message (princ-to-string e))))))

(defun eval-with-output (code &key (package :cl-user))
  "Evaluate code and capture stdout/stderr."
  (let ((*package* (find-package package)))
    (let ((output (make-string-output-stream)))
      (let ((*standard-output* output)
            (*error-output* output))
        (let ((result (safe-eval code :package package)))
          (list :result result
                :output (get-output-stream-string output)))))))
