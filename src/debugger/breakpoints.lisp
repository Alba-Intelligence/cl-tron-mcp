;;;; src/debugger/breakpoints.lisp

(in-package :cl-tron-mcp/debugger)

(defvar *breakpoints* (make-hash-table :test 'equal))
(defvar *next-breakpoint-id* 1)

(defun set-breakpoint (function-name &key condition hit-count thread)
  "Set breakpoint on a function via Swank RPC."
  (handler-case
      (let ((result (swank-set-breakpoint
                     :function function-name
                     :condition condition
                     :hit-count hit-count
                     :thread thread)))
        (if (getf result :error)
            result
            (let* ((id *next-breakpoint-id*))
              (incf *next-breakpoint-id*)
              (let ((breakpoint-data (list :id id
                                           :function function-name
                                           :condition condition
                                           :hit-count (or hit-count 0)
                                           :thread thread
                                           :enabled t
                                           :swank-id (getf result :breakpoint-id))))
                (setf (gethash id *breakpoints*) breakpoint-data)
                (list :breakpoint-id id
                      :function function-name
                      :status "active"
                      :message "Breakpoint set via Swank")))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun remove-breakpoint (breakpoint-id)
  "Remove breakpoint by ID via Swank RPC."
  (handler-case
      (let ((bp (gethash breakpoint-id *breakpoints*)))
        (unless bp
          (return-from remove-breakpoint
            (list :error t
                  :message (format nil "Breakpoint ~d not found" breakpoint-id))))
        (let ((swank-id (getf bp :swank-id)))
          (when swank-id
            (let ((result (swank-remove-breakpoint :breakpoint-id swank-id)))
              (when (getf result :error)
                (return-from remove-breakpoint result)))))
        (remhash breakpoint-id *breakpoints*)
        (list :breakpoint-id breakpoint-id
              :status "removed"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun list-breakpoints ()
  "List all active breakpoints via Swank RPC."
  (handler-case
      (let ((result (swank-list-breakpoints)))
        (if (getf result :error)
            result
            (list :breakpoints (getf result :breakpoints)
                  :count (length (getf result :breakpoints)))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun toggle-breakpoint (breakpoint-id)
  "Toggle breakpoint enabled/disabled state.
When disabling: removes the breakpoint from Swank but preserves local state.
When enabling: re-installs the breakpoint in Swank using saved parameters."
  (handler-case
      (let ((bp (gethash breakpoint-id *breakpoints*)))
        (unless bp
          (return-from toggle-breakpoint
            (list :error t
                  :message (format nil "Breakpoint ~d not found" breakpoint-id))))
        (let ((enabled (getf bp :enabled)))
          (if enabled
              ;; Disabling: remove from Swank, keep local state
              (let ((swank-id (getf bp :swank-id)))
                (when swank-id
                  (let ((result (swank-remove-breakpoint :breakpoint-id swank-id)))
                    (when (getf result :error)
                      (return-from toggle-breakpoint result))))
                (setf (getf bp :enabled) nil)
                (setf (getf bp :swank-id) nil)
                (list :breakpoint-id breakpoint-id :enabled nil :status "disabled"))
              ;; Enabling: re-install in Swank
              (let ((result (swank-set-breakpoint
                             :function (getf bp :function)
                             :condition (getf bp :condition)
                             :hit-count (when (plusp (or (getf bp :hit-count) 0))
                                          (getf bp :hit-count))
                             :thread (getf bp :thread))))
                (if (getf result :error)
                    result
                    (progn
                      (setf (getf bp :enabled) t)
                      (setf (getf bp :swank-id) (getf result :breakpoint-id))
                      (list :breakpoint-id breakpoint-id :enabled t :status "active")))))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

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
