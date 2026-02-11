;;;; src/sbcl/threads.lisp

(in-package :cl-tron-mcp/sbcl)

(defun find-thread (thread-id)
  "Find thread by ID string."
  (let ((threads (bt:all-threads)))
    (find thread-id threads
          :test (lambda (id thread)
                  (string= id (format nil "~a" (bt:thread-name thread)))))))

(defun list-threads ()
  "List all threads with status."
  (mapcar (lambda (thread)
            (list :id (format nil "~a" (bt:thread-name thread))
                  :name (bt:thread-name thread)
                  :state (if (bt:thread-alive-p thread) :running :dead)))
          (bt:all-threads)))

(defun get-thread-state (thread)
  "Get detailed state of a thread."
  (list :id (format nil "~a" (bt:thread-name thread))
        :name (bt:thread-name thread)))

(defun thread-state (thread)
  "Get thread state keyword."
  (if (bt:thread-alive-p thread)
      :running
      :dead))
