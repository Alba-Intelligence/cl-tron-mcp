;;;; src/sbcl/compile.lisp

(in-package :cl-tron-mcp/sbcl)

(defun safe-compile (code &key (filename "repl"))
  "Compile Lisp code string to FASL."
  (handler-case
      (let ((form (read-from-string code)))
        (let ((fasl (compile nil form)))
          (list :success t
                :fasl fasl)))
    (error (e)
      (list :error t
            :type "COMPILATION-ERROR"
            :message (princ-to-string e)))))

(defun safe-load-fasl (fasl)
  "Load compiled FASL into image."
  (handler-case
      (progn
        (load fasl)
        (list :success t))
    (error (e)
      (list :error t
            :type "LOAD-ERROR"
            :message (princ-to-string e)))))
