;;;; src/tracer/core.lisp

(in-package :cl-tron-mcp/tracer)

(defvar *traced-functions* nil)
(defvar *trace-entries* nil)
(defvar *trace-count* 0)
(defvar *max-trace-entries* 1000)

(defun trace-function (function-name &key condition hit-count)
  "Add trace to function."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          #+sbcl (eval `(trace ,symbol))
          (pushnew symbol *traced-functions*)
          (list :success t
                :function function-name
                :condition condition
                :hit-count (or hit-count 0)))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-remove (function-name)
  "Remove trace from function."
  (let ((symbol (read-from-string function-name)))
    (handler-case
        (progn
          #+sbcl (eval `(untrace ,symbol))
          (setq *traced-functions* (remove symbol *traced-functions*))
          (list :success t
                :function function-name))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun trace-list ()
  "List all traced functions."
  (list :traced *traced-functions*
        :count (length *traced-functions*)))

(defun trace-clear ()
  "Remove all traces."
  (handler-case
      (progn
        #+sbcl (eval '(untrace))
        (setq *traced-functions* nil
              *trace-entries* nil
              *trace-count* 0)
        (list :success t
              :message "All traces cleared"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun trace-get-entries (&key (limit 100))
  "Get trace entries with call information."
  (let ((entries (reverse *trace-entries*)))
    (list :entries (subseq entries 0 (min limit (length entries)))
          :total (length *trace-entries*)
          :limit limit)))

(defun trace-capture-entry (function-name args result)
  "Capture a trace entry for recording."
  (when (< *trace-count* *max-trace-entries*)
    (push (list :function function-name
                :args (format nil "~a" args)
                :result (format nil "~a" result)
                :timestamp (get-unix-time)
                :index (incf *trace-count*))
          *trace-entries*)))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (+ (* sec 1)
       (* min 60)
       (* hour 3600)
       (* day 86400)
       (* month 2592000)
       (* year 31536000))))
