;;;; src/debugger/frames.lisp

(in-package :cl-tron-mcp/debugger)

(defun get-debugger-frames (&key (thread nil) (start 0) (end 20))
  "Get debugger stack frames using SB-DI when available."
  (declare (ignorable thread))
  (handler-case
      (progn
        #+sb-dbg
        (if (boundp 'sb-debug:*stack-trace-depth*)
            (let ((frames (loop for i from start below (min end sb-debug:*stack-trace-depth*)
                                for frame = (sb-di:top-frame)
                                then (sb-di:frame-down frame i)
                                while frame
                                collect (list :index i
                                              :function (or (sb-di:debug-fun-name (sb-di:frame-debug-function frame))
                                                            (format nil "frame-~d" i))
                                              :source (ignore-errors
                                                       (let ((source (sb-di:frame-source-location frame)))
                                                         (when source
                                                           (list :file (namestring (sb-c:definition-source-pathname source))
                                                                 :line (sb-c:definition-source-line source)
                                                                 :column (sb-c:definition-source-column source))))))))
              (list :frames frames
                    :total sb-debug:*stack-trace-depth*))
            (list :frames nil :total 0 :message "Debugger not active"))
        #-sb-dbg
        (let ((frames (loop for i from start below end
                            collect (list :index i
                                          :function (format nil "frame-~d" i)
                                          :source (list :file "unknown" :line 0 :column 0)))))
          (list :frames frames
                :total (length frames)
                :note "SBCL compiled without debug support"))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun get-frame-locals (&key (frame 0))
  "Get local variables in frame."
  (declare (ignorable frame))
  #+sb-dbg
  (handler-case
      (progn
        (when (boundp 'sb-debug:*stack-trace-depth*)
          (let* ((current-frame (loop for f = (sb-di:top-frame) then (sb-di:frame-down f 1)
                                      repeat frame
                                      finally (return f)))
                 (locals (ignore-errors
                          (loop for i from 0 below (sb-di:debug-var-count current-frame)
                                for var = (sb-di:debug-var-info i current-frame)
                                when (sb-di:debug-var-valid-p var current-frame)
                                collect (list :name (sb-di:debug-var-name var)
                                              :value (format nil "~a" (sb-di:debug-var-value var current-frame))
                                              :id i)))))
            (list :frame frame :locals locals))))
    (list :locals nil :message "Unable to retrieve locals"))
  #-sb-dbg
  (list :locals nil :note "SBCL compiled without debug support"))

(defun eval-in-frame (frame code &key (package :cl-user))
  "Evaluate code in frame context."
  (declare (ignorable frame package))
  #+sb-dbg
  (handler-case
      (progn
        (when (boundp 'sb-debug:*stack-trace-depth*)
          (let ((current-frame (loop for f = (sb-di:top-frame) then (sb-di:frame-down f 1)
                                      repeat frame
                                      finally (return f))))
            (cl-tron-mcp/sbcl:safe-eval code :package package))))
    (list :error t :message "Cannot evaluate in frame outside debugger context"))
  #-sb-dbg
  (cl-tron-mcp/sbcl:safe-eval code :package package))
