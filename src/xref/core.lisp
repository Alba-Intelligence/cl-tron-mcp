;;;; src/xref/core.lisp
;;;;
;;;; Cross-reference queries.
;;;;
;;;; On SBCL: uses sb-introspect for precise compile-time xref data.
;;;; On ECL and other implementations: delegates to Swank xref RPC when
;;;; connected (swank:who-calls etc.), which Swank supports portably.

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(in-package :cl-tron-mcp/xref)

(defun xref-unavailable (symbol-name message)
  (list :symbol symbol-name :error t :message message))

(defun swank-xref (xref-type symbol-name)
  "Delegate xref query to Swank RPC. Works on any implementation with Swank connected."
  (handler-case
      (let* ((swank-eval-fn (find-symbol "SWANK-EVAL" :cl-tron-mcp/swank))
             (sym (read-from-string symbol-name)))
        (unless swank-eval-fn
          (return-from swank-xref
            (xref-unavailable symbol-name "Swank not loaded — cannot perform xref on this implementation.")))
        (let ((result (funcall swank-eval-fn
                               :code (format nil "(swank:xref '~a '~a)" xref-type sym)
                               :package "CL-USER")))
          (if (getf result :error)
              result
              (list :symbol symbol-name
                    :results (getf result :value)
                    :source :swank))))
    (error (e)
      (xref-unavailable symbol-name (princ-to-string e)))))

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
    (swank-xref :calls symbol-name)))

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
    (swank-xref :references symbol-name)))

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
    (swank-xref :binds symbol-name)))

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
    (swank-xref :sets symbol-name)))

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
    (swank-xref :calls-who symbol-name)))

(defun list-callers (symbol-name)
  (who-calls symbol-name))
