;;;; src/sbcl/eval.lisp

(in-package :cl-tron-mcp/sbcl)

(defvar *eval-timeout* 30)

(defun safe-eval (code &key (package :cl-user) (timeout *eval-timeout*) (safe-read t))
  "Evaluate Lisp code safely with optional timeout and error handling.
When TIMEOUT is positive, evaluation is interrupted if it exceeds TIMEOUT seconds."
  (let ((*package* (or (find-package package)
                       (error "Package ~a not found" package))))
    (handler-case
        (flet ((do-eval ()
                 (let ((form (read-from-string code)))
                   (when safe-read
                     (setf *read-eval* nil))
                   (eval form))))
          (if (and timeout (plusp timeout))
              (let ((result-cell (list nil))
                    (error-cell (list nil))
                    (done nil))
                (let ((eval-thread
                        (bt:make-thread
                         (lambda ()
                           (handler-case (setf (car result-cell) (do-eval))
                             (error (e) (setf (car error-cell) e)))
                           (setf done t))
                         :name "safe-eval")))
                  (loop with start = (get-universal-time)
                        until (or done (> (- (get-universal-time) start) timeout))
                        do (sleep 0.05))
                  (cond
                    (done
                     (if (car error-cell)
                         (list :error t
                               :type (prin1-to-string (type-of (car error-cell)))
                               :message (princ-to-string (car error-cell)))
                         (car result-cell)))
                    (t
                     (ignore-errors (bt:destroy-thread eval-thread))
                     (list :error t
                           :type "TIMEOUT"
                           :message (format nil "Evaluation timed out after ~d seconds" timeout))))))
              (do-eval)))
      (reader-error (e)
        (list :error t
              :type "READER-ERROR"
              :message (princ-to-string e)))
      (error (e)
        (list :error t
              :type (prin1-to-string (type-of e))
              :message (princ-to-string e))))))

(defun eval-with-output (code &key (package :cl-user) (timeout *eval-timeout*))
  "Evaluate code and capture stdout/stderr."
  (let ((*package* (find-package package)))
    (let ((output (make-string-output-stream)))
      (let ((*standard-output* output)
            (*error-output* output))
        (let ((result (safe-eval code :package package :timeout timeout)))
          (list :result result
                :output (get-output-stream-string output)))))))
