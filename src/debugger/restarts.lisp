;;;; src/debugger/restarts.lisp

(in-package :cl-tron-mcp/debugger)

(defun list-restarts (&key (thread nil))
  "List available restarts."
  (declare (ignorable thread))
  (list :restarts
        (list (list :name "ABORT"
                    :description "Return to top level")
              (list :name "USE-VALUE"
                    :description "Use provided value"))))

(defun invoke-named-restart (restart-name &rest args)
  "Invoke a restart by name."
  (declare (ignorable restart-name args))
  (list :result "restart invoked"))
