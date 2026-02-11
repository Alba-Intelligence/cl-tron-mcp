;;;; src/debugger/stepping.lisp

(in-package :cl-tron-mcp/debugger)

(defvar *stepping-mode* nil)
(defvar *stepping-target* nil)

(defun step-frame (frame &key (mode :into))
  "Step execution in frame. MODE can be :into, :over, or :out."
  (declare (ignore frame))
  (setq *stepping-mode* mode)
  (list :mode mode
        :status "stepping"
        :message (case mode
                   (:into "Stepping into next function call")
                   (:over "Stepping over next expression")
                   (:out "Stepping out of current frame"))))

(defun step-into (&optional frame)
  "Step into the next function call."
  (step-frame frame :mode :into))

(defun step-over (&optional frame)
  "Step over the next expression."
  (step-frame frame :mode :over))

(defun step-out (&optional frame)
  "Step out of the current frame."
  (step-frame frame :mode :out))

(defun continue-execution ()
  "Continue normal execution."
  (setq *stepping-mode* nil)
  (setq *stepping-target* nil)
  (list :status "running"))

(defun get-stepping-state ()
  "Get current stepping state."
  (list :stepping (if *stepping-mode* t nil)
        :mode *stepping-mode*
        :target *stepping-target*))
