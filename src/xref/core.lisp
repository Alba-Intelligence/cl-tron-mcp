;;;; src/xref/core.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(in-package :cl-tron-mcp/xref)

(defun who-calls (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    (handler-case
        (let ((callers (ignore-errors (sb-introspect:who-calls symbol))))
          (list :symbol symbol-name
                :callers (mapcar #'string callers)
                :count (length callers)))
      (error (e)
        (list :error t :message (princ-to-string e))))))

(defun who-references (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    (handler-case
        (let ((refs (ignore-errors (sb-introspect:who-references symbol))))
          (list :symbol symbol-name
                :references (mapcar #'string refs)
                :count (length refs)))
      (error (e)
        (list :error t :message (princ-to-string e))))))

(defun who-binds (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    (handler-case
        (let ((binds (ignore-errors (sb-introspect:who-binds symbol))))
          (list :symbol symbol-name
                :bindings (mapcar #'string binds)
                :count (length binds)))
      (error (e)
        (list :error t :message (princ-to-string e))))))

(defun who-sets (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    (handler-case
        (let ((sets (ignore-errors (sb-introspect:who-sets symbol))))
          (list :symbol symbol-name
                :sets (mapcar #'string sets)
                :count (length sets)))
      (error (e)
        (list :error t :message (princ-to-string e))))))

(defun list-callees (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    (handler-case
        (let ((callees (ignore-errors (sb-introspect:find-function-callees symbol))))
          (list :symbol symbol-name
                :callees (mapcar #'string callees)
                :count (length callees)))
      (error (e)
        (list :error t :message (princ-to-string e))))))

(defun list-callers (symbol-name)
  (who-calls symbol-name))
