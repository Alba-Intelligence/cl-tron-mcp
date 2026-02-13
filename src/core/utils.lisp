;;;; src/core/utils.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :jonathan :silent t))

(in-package :cl-tron-mcp/core)

(defun safe-json-encode (object)
  "Encode object to JSON string, handling circular references."
  (handler-case
      (jonathan:to-json object)
    (error (e)
      (format nil "~a" object))))

(defun timestamp ()
  "Return current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun format-error (condition)
  "Format condition for JSON response."
  (list :type (prin1-to-string (type-of condition))
        :message (princ-to-string condition)))

(defmacro with-timing (&body body)
  "Measure execution time of body."
  (let ((start (gensym "START"))
        (end (gensym "END"))
        (result (gensym "RESULT")))
    `(let ((,start (get-internal-real-time)))
       (let ((,result (progn ,@body)))
         (let ((,end (get-internal-real-time)))
           (values ,result
                   (/ (- ,end ,start) internal-time-units-per-second)))))))
