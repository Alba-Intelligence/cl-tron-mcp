;;;; src/sbcl/threads.lisp

(in-package :cl-tron-mcp/sbcl)

(defvar *thread-inspections* (make-hash-table :test 'equal))

(defun find-thread (thread-id)
  "Find thread by ID string."
  (let ((threads (bt:all-threads)))
    (find thread-id threads
          :test (lambda (id thread)
                  (string= id (format nil "~a" (bt:thread-name thread)))))))

(defun list-threads ()
  "List all threads with detailed status."
  (mapcar (lambda (thread)
            (let ((id (format nil "~a" (bt:thread-name thread))))
              (list :id id
                    :name (bt:thread-name thread)
                    :state (thread-state thread)
                    :alive (bt:thread-alive-p thread))))
          (bt:all-threads)))

(defun get-thread-state (thread)
  "Get detailed state of a thread."
  (let ((thr (find-thread thread)))
    (when thr
      (list :id (format nil "~a" (bt:thread-name thr))
            :name (bt:thread-name thr)
            :alive (bt:thread-alive-p thr)
            :state (thread-state thr)))))

(defun thread-state (thread)
  "Get thread state keyword."
  (if (bt:thread-alive-p thread)
      :running
      :dead))

(defun inspect-thread (thread-id)
  "Get detailed information about a thread."
  (let ((thread (find-thread thread-id)))
    (unless thread
      (return-from inspect-thread
        (list :error t
              :message (format nil "Thread ~a not found" thread-id))))
    (let ((info (list :id thread-id
                      :name (bt:thread-name thread)
                      :alive (bt:thread-alive-p thread)
                      :state (thread-state thread)
                      :thread-object thread)))
      (setf (gethash thread-id *thread-inspections*) info)
      info)))

(defun thread-backtrace (thread-id)
  "Get backtrace for a specific thread."
  (let ((thread (find-thread thread-id)))
    (unless thread
      (return-from thread-backtrace
        (list :error t
              :message (format nil "Thread ~a not found" thread-id))))
    (list :thread-id thread-id
          :backtrace nil
          :note "Thread backtrace requires debugger context")))
