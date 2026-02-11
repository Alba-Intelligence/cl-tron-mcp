;;;; src/debugger/breakpoints.lisp

(in-package :cl-tron-mcp/debugger)

(defvar *breakpoints* (make-hash-table :test 'equal))
(defvar *next-breakpoint-id* 1)

(defun set-breakpoint (function-name &key condition hit-count thread)
  "Set breakpoint on a function (stores metadata for tracking)."
  (handler-case
      (let* ((id *next-breakpoint-id*))
        (incf *next-breakpoint-id*)
        (let ((breakpoint-data (list :id id
                                     :function function-name
                                     :condition condition
                                     :hit-count (or hit-count 0)
                                     :thread thread
                                     :enabled t)))
          (setf (gethash id *breakpoints*) breakpoint-data)
          (list :breakpoint-id id
                :function function-name
                :status "active"
                :message "Breakpoint registered (SBCL breakpoint facility requires sb-sprof or debug policy)")))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun remove-breakpoint (breakpoint-id)
  "Remove breakpoint by ID."
  (handler-case
      (let ((bp (gethash breakpoint-id *breakpoints*)))
        (unless bp
          (return-from remove-breakpoint
            (list :error t
                  :message (format nil "Breakpoint ~d not found" breakpoint-id))))
        (remhash breakpoint-id *breakpoints*)
        (list :breakpoint-id breakpoint-id
              :status "removed"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun list-breakpoints ()
  "List all active breakpoints."
  (let ((bps (loop for id being the hash-keys of *breakpoints*
                   collect (gethash id *breakpoints*))))
    (list :breakpoints bps
          :count (length bps))))

(defun toggle-breakpoint (breakpoint-id)
  "Toggle breakpoint enabled/disabled state."
  (let ((bp (gethash breakpoint-id *breakpoints*)))
    (unless bp
      (return-from toggle-breakpoint
        (list :error t
              :message (format nil "Breakpoint ~d not found" breakpoint-id))))
    (let ((enabled (getf bp :enabled)))
      (setf (getf bp :enabled) (not enabled))
      (list :breakpoint-id breakpoint-id
            :enabled (getf bp :enabled)))))

(defun get-breakpoint-info (breakpoint-id)
  "Get detailed information about a breakpoint."
  (let ((bp (gethash breakpoint-id *breakpoints*)))
    (unless bp
      (return-from get-breakpoint-info
        (list :error t
              :message (format nil "Breakpoint ~d not found" breakpoint-id))))
    (list :breakpoint-id breakpoint-id
          :data bp
          :status (if (getf bp :enabled) "active" "disabled"))))
