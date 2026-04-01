;;;; src/profiler/core.lisp

(in-package :cl-tron-mcp/profiler)

(defvar *profiling-active* nil)
(defvar *profile-data* nil)

(defun profile-start (&key functions package)
  "Start deterministic profiling via sb-profile (SBCL) or stub."
  (declare (ignore functions package))
  #+sbcl
  (handler-case
      (progn
        (unless *profiling-active*
          (sb-profile:profile)
          (setq *profiling-active* t
                *profile-data* (list :started (get-unix-time))))
        (list :success t
              :started *profiling-active*
              :message (if *profiling-active* "Profiling started" "Already active")))
    (error (e)
      (list :error t
            :message (princ-to-string e))))
  #-sbcl
  (list :success t :message "Profiling not available on this Lisp (stub)"))

(defun profile-stop ()
  "Stop profiling."
  #+sbcl
  (handler-case
      (progn
        (when *profiling-active*
          (sb-profile:unprofile)
          (setq *profiling-active* nil))
        (list :success t :message "Profiling stopped"))
    (error (e)
      (list :error t
            :message (princ-to-string e))))
  #-sbcl
  (progn
    (setq *profiling-active* nil)
    (list :success t :message "Profiling stopped (stub)")))

(defun profile-report (&key (format :flat))
  "Get profiling report."
  (when *profile-data*
    (handler-case
        #-sbcl
      (list :error t
            :message "Profiling not available")
      #+sbcl
      (let ((output (make-string-output-stream)))
        (sb-profile:report :stream output)
        (list :report (get-output-stream-string output)
              :format format))
      (error (e)
        (list :error t
              :message (princ-to-string e))))))

(defun profile-reset ()
  "Reset profile counters."
  (handler-case
      (progn
        #+sbcl (sb-profile:reset)
        (list :success t
              :message "Profile counters reset"))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun sprof-start (&key (max-samples 1000) (interval 0.001))
  "Start statistical profiling."
  (declare (ignore max-samples interval))
  (list :error t
        :message "Statistical profiling not available in this SBCL build"))

(defun sprof-report ()
  "Get statistical profiling report."
  (list :error t
        :message "Statistical profiling not available"))

(defun profile-flamegraph (&key (output-path "/tmp/flamegraph.svg") sample-count)
  "Generate flamegraph SVG."
  (declare (ignore output-path sample-count))
  (list :error t
        :message "Flamegraph generation not yet implemented"))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))
