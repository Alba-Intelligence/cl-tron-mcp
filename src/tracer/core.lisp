;;;; src/tracer/core.lisp
;;;; CL:TRACE and CL:UNTRACE are ANSI standard — works on SBCL, ECL, and all
;;;; conforming implementations. No implementation guards needed.

(in-package :cl-tron-mcp/tracer)

(defvar *traced-functions* nil)
(defvar *trace-entries* nil)
(defvar *trace-count* 0)
(defvar *max-trace-entries* 1000)

(defun trace-function (function-name &key condition hit-count)
  "Add trace to function. Uses standard CL:TRACE — works on all implementations."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          (eval `(trace ,symbol))
          (pushnew symbol *traced-functions*)
          (list :success t
                :function function-name
                :condition condition
                :hit-count (or hit-count 0)))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-remove (function-name)
  "Remove trace from function. Uses standard CL:UNTRACE."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          (eval `(untrace ,symbol))
          (setq *traced-functions* (remove symbol *traced-functions*))
          (list :success t
                :function function-name))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-list ()
  "List all traced functions."
  (list :traced *traced-functions*
        :count (length *traced-functions*)))

(defun trace-clear ()
  "Remove all traces. Uses standard CL:UNTRACE."
  (handler-case
      (progn
        (eval '(untrace))
        (setq *traced-functions* nil
              *trace-entries* nil
              *trace-count* 0)
        (list :success t
              :message "All traces cleared"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))