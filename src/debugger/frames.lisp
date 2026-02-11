;;;; src/debugger/frames.lisp

(in-package :cl-tron-mcp/debugger)

(defun get-debugger-frames (&key (thread nil) (start 0) (end 20))
  "Get debugger stack frames."
  (declare (ignorable thread))
  (handler-case
      (let ((frames (loop for i from 0 below end
                          collect (list :index i
                                        :function (format nil "frame-~d" i)
                                        :source (list :file "unknown"
                                                      :line 0
                                                      :column 0)))))
        (list :frames frames
              :total (length frames)))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun get-frame-locals (&key (frame 0))
  "Get local variables in frame."
  (declare (ignorable frame))
  (list :locals nil))

(defun eval-in-frame (frame code &key (package :cl-user))
  "Evaluate code in frame context."
  (declare (ignorable frame package))
  (cl-tron-mcp/sbcl:safe-eval code :package package))
