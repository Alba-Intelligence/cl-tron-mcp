;;;; src/debugger/stepping.lisp

(in-package :cl-tron-mcp/debugger)

(defvar *stepping-mode* nil)
(defvar *stepping-target* nil)

(defun step-frame (frame &key (mode :into))
  "Step execution in frame via Swank connection. MODE can be :into, :over, or :out."
  (handler-case
      (let ((result (ecase mode
                      (:into (swank-step frame))
                      (:over (swank-next frame))
                      (:out (swank-out frame)))))
        (if (getf result :error)
            result
            (progn
              (setq *stepping-mode* mode)
              (list :mode mode
                    :status "stepping"
                    :message (case mode
                               (:into "Stepping into next function call")
                               (:over "Stepping over next expression")
                               (:out "Stepping out of current frame"))))))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun step-into (&optional frame)
  "Step into the next function call."
  (step-frame (or frame 0) :mode :into))

(defun step-over (&optional frame)
  "Step over the next expression."
  (step-frame (or frame 0) :mode :over))

(defun step-out (&optional frame)
  "Step out of the current frame."
  (step-frame (or frame 0) :mode :out))

(defun continue-execution ()
  "Continue normal execution via Swank connection."
  (handler-case
      (let ((result (swank-continue)))
        (if (getf result :error)
            result
            (progn
              (setq *stepping-mode* nil)
              (setq *stepping-target* nil)
              (list :status "running"))))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun get-stepping-state ()
  "Get current stepping state."
  (list :stepping (if *stepping-mode* t nil)
        :mode *stepping-mode*
        :target *stepping-target*))
