;;;; src/debugger/stepping.lisp

(in-package :cl-tron-mcp/debugger)

(defun step-frame (frame &key (mode :into))
  "Step execution in frame."
  (declare (ignorable frame mode))
  (list :result "stepped"))
