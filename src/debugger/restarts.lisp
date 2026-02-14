;;;; src/debugger/restarts.lisp

(in-package :cl-tron-mcp/debugger)

(defun list-restarts (&key (thread nil))
  "List available restarts via Swank connection."
  (declare (ignorable thread))
  (handler-case
      (let ((result (swank-get-restarts)))
        (if (getf result :error)
            result
            (list :restarts (getf result :restarts)
                  :count (length (getf result :restarts)))))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun invoke-named-restart (restart-name &rest args)
  "Invoke a restart by name via Swank connection."
  (handler-case
      (let ((result (swank-invoke-restart restart-name)))
        (declare (ignore args))
        (if (getf result :error)
            result
            (list :name restart-name :status "invoked" :result (getf result :result))))
    (error (e)
      (list :error t :message (princ-to-string e)))))
