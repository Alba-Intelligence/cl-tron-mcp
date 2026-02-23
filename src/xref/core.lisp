;;;; src/xref/core.lisp

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(in-package :cl-tron-mcp/xref)

(defun xref-unavailable (symbol-name message)
  (list :symbol symbol-name :error t :message message))

(defun who-calls (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sbcl
    (handler-case
        (let ((callers (ignore-errors (sb-introspect:who-calls symbol))))
          (list :symbol symbol-name
                :callers (mapcar #'string callers)
                :count (length callers)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sbcl
    (xref-unavailable symbol-name "who_calls requires SBCL (sb-introspect).")))

(defun who-references (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sbcl
    (handler-case
        (let ((refs (ignore-errors (sb-introspect:who-references symbol))))
          (list :symbol symbol-name
                :references (mapcar #'string refs)
                :count (length refs)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sbcl
    (xref-unavailable symbol-name "who_references requires SBCL (sb-introspect).")))

(defun who-binds (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sbcl
    (handler-case
        (let ((binds (ignore-errors (sb-introspect:who-binds symbol))))
          (list :symbol symbol-name
                :bindings (mapcar #'string binds)
                :count (length binds)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sbcl
    (xref-unavailable symbol-name "who_binds requires SBCL (sb-introspect).")))

(defun who-sets (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sbcl
    (handler-case
        (let ((sets (ignore-errors (sb-introspect:who-sets symbol))))
          (list :symbol symbol-name
                :sets (mapcar #'string sets)
                :count (length sets)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sbcl
    (xref-unavailable symbol-name "who_sets requires SBCL (sb-introspect).")))

(defun list-callees (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sbcl
    (handler-case
        (let ((callees (ignore-errors (sb-introspect:find-function-callees symbol))))
          (list :symbol symbol-name
                :callees (mapcar #'string callees)
                :count (length callees)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sbcl
    (xref-unavailable symbol-name "list_callees requires SBCL (sb-introspect).")))

(defun list-callers (symbol-name)
  (who-calls symbol-name))
