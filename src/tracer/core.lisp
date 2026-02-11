;;;; src/tracer/core.lisp

(in-package :cl-tron-mcp/tracer)

(defvar *traced-functions* nil)
(defvar *trace-entries* nil)

(defun trace-function (function-name &key condition hit-count)
  "Add trace to function."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          #+sbcl (trace symbol)
          (pushnew symbol *traced-functions*)
          (list :success t
                :function function-name))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-remove (function-name)
  "Remove trace from function."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          #+sbcl (untrace symbol)
          (setq *traced-functions* (remove symbol *traced-functions*))
          (list :success t
                :function function-name))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-list ()
  "List all traced functions."
  (list :traced *traced-functions*))

(defun trace-clear ()
  "Remove all traces."
  (handler-case
      (progn
        #+sbcl (untrace :all)
        (setq *traced-functions* nil)
        (list :success t
              :message "All traces cleared"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun trace-get-entries (&key (limit 100))
  "Get trace entries. Placeholder."
  (declare (ignore limit))
  (list :entries *trace-entries*))
