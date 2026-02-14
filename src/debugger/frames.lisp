;;;; src/debugger/frames.lisp

(in-package :cl-tron-mcp/debugger)

(defun get-debugger-frames (&key (thread nil) (start 0) (end 20))
  "Get debugger stack frames via Swank connection."
  (declare (ignorable thread))
  (handler-case
      (let ((result (swank-backtrace :start start :end end)))
        (if (getf result :error)
            result
            (list :frames (getf result :frames)
                  :total (getf result :total))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun get-frame-locals (&key (frame 0))
  "Get local variables in frame via Swank connection."
  (handler-case
      (let ((result (swank-frame-locals frame)))
        (if (getf result :error)
            result
            (list :frame frame :locals (getf result :locals))))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun eval-in-frame (frame code &key (package :cl-user))
  "Evaluate code in frame context via Swank connection."
  (declare (ignorable frame package))
  (handler-case
      (let ((result (swank-eval-in-frame code frame :package package)))
        (if (getf result :error)
            result
            (list :frame frame :result (getf result :result))))
    (error (e)
      (list :error t :message (princ-to-string e)))))
