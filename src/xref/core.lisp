;;;; src/xref/core.lisp

(in-package :cl-tron-mcp/xref)

(defun who-calls (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((callers (ignore-errors (sb-introspect:who-calls symbol))))
          (list :symbol symbol-name
                :callers (mapcar #'string callers)
                :count (length callers)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun who-references (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((refs (ignore-errors (sb-introspect:who-references symbol))))
          (list :symbol symbol-name
                :references (mapcar #'string refs)
                :count (length refs)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun who-binds (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((binds (ignore-errors (sb-introspect:who-binds symbol))))
          (list :symbol symbol-name
                :bindings (mapcar #'string binds)
                :count (length binds)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun who-sets (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((sets (ignore-errors (sb-introspect:who-sets symbol))))
          (list :symbol symbol-name
                :sets (mapcar #'string sets)
                :count (length sets)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun who-specializes (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((specs (ignore-errors (sb-introspect:who-specializes symbol))))
          (list :symbol symbol-name
                :specializers (mapcar #'string specs)
                :count (length specs)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun who-macroexpands (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((macros (ignore-errors (sb-introspect:who-macroexpands symbol))))
          (list :symbol symbol-name
                :macroexpands (mapcar #'string macros)
                :count (length macros)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun list-callees (symbol-name)
  (let ((symbol (read-from-string symbol-name)))
    #+sb-introspect
    (handler-case
        (let ((callees (ignore-errors (sb-introspect:list-callees symbol))))
          (list :symbol symbol-name
                :callees (mapcar #'string callees)
                :count (length callees)))
      (error (e)
        (list :error t :message (princ-to-string e))))
    #-sb-introspect
    (list :error t :message "Cross-referencing requires SBCL with sb-introspect")))

(defun list-callers (symbol-name)
  (who-calls symbol-name))
