;;;; src/debugger/breakpoints.lisp

(in-package :cl-tron-mcp/debugger)

(defvar *breakpoints* (make-hash-table))
(defvar *next-breakpoint-id* 1)

(defun set-breakpoint (function &key condition hit-count thread)
  "Set breakpoint on function."
  (let ((id *next-breakpoint-id*))
    (incf *next-breakpoint-id*)
    (setf (gethash id *breakpoints*)
          (list :id id
                :function function
                :condition condition
                :hit-count hit-count
                :thread thread
                :enabled t))
    (list :breakpoint-id id)))

(defun remove-breakpoint (breakpoint-id)
  "Remove breakpoint by ID."
  (remhash breakpoint-id *breakpoints*)
  (list :result "removed"))

(defun list-breakpoints ()
  "List all active breakpoints."
  (let ((bps (loop for id being the hash-keys of *breakpoints*
                   collect (gethash id *breakpoints*))))
    (list :breakpoints bps)))
