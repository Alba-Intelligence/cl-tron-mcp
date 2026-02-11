;;;; src/repl/core.lisp

(in-package :cl-tron-mcp/repl)

(defun repl-eval (code &key (package :cl-user) (timeout 30) (print-level 10) (print-length 100))
  "Evaluate Lisp code in REPL context."
  (let ((*package* (or (find-package package)
                      (error "Package ~a not found" package))))
    (handler-case
        (let ((*print-level* print-level)
              (*print-length* print-length)
              (*read-eval* nil))
          (let ((form (read-from-string code)))
            (let ((result (eval form)))
              (list :result (format nil "~a" result)))))
      (reader-error (e)
        (list :error t
              :type "READER-ERROR"
              :message (princ-to-string e)))
      (error (e)
        (list :error t
              :type "EVAL-ERROR"
              :message (princ-to-string e))))))

(defun repl-compile-and-load (code &key (filename "repl"))
  "Compile and load Lisp code."
  (handler-case
      (let ((form (read-from-string code)))
        (let ((fasl (compile nil form)))
          (when fasl
            (load fasl))
          (list :success t
                :message "Code compiled and loaded")))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))
