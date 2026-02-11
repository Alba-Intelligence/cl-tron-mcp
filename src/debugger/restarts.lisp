;;;; src/debugger/restarts.lisp

(in-package :cl-tron-mcp/debugger)

(defun list-restarts (&key (thread nil))
  "List available restarts."
  (declare (ignorable thread))
  (let ((restarts
          #+sb-dbg
          (handler-case
              (if (and (boundp 'sb-debug:*stack-trace-depth*)
                       (plusp sb-debug:*stack-trace-depth*))
                  (loop for r in (compute-restarts)
                        collect (list :name (restart-name r)
                                      :description (or (format nil "~a" r) "No description")))
              (error () nil))
          #-sb-dbg
          (list (list :name "ABORT" :description "Return to top level")
                (list :name "USE-VALUE" :description "Use provided value")
                (list :name "MUFFLE-WARNING" :description "Proceed with warning muffled")
                (list :name "CONTINUE" :description "Continue from condition")))))
    #+sb-dbg
    (if restarts
        (list :restarts restarts :count (length restarts))
        (list :restarts nil :message "No debugger context active"))
    #-sb-dbg
    (list :restarts restarts
          :count (length restarts)
          :note "SBCL compiled without debug support")))

(defun invoke-named-restart (restart-name &rest args)
  "Invoke a restart by name."
  #+sb-dbg
  (handler-case
      (let ((restart (find restart-name (compute-restarts) :key #'restart-name :test #'string=)))
        (if restart
            (progn
              (apply #'invoke-restart restart args)
              (list :name restart-name :status "invoked"))
            (list :error t :message (format nil "Restart ~a not found" restart-name))))
    (error (e)
      (list :error t :message (princ-to-string e))))
  #-sb-dbg
  (list :status "invoked"
        :note (format nil "Restart ~a would be invoked (debugger not active)" restart-name)))
