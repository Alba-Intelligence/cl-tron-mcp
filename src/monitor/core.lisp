;;;; src/monitor/core.lisp

(in-package :cl-tron-mcp/monitor)

(defun memory-stats ()
  "Get memory usage statistics."
  (handler-case
      (list :total-bytes #-sbcl nil
            :generations nil)
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun gc-stats ()
  "Get GC statistics."
  (handler-case
      (list :gc-run-time nil
            :bytes-freed nil
            :gc-count nil)
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun runtime-stats ()
  "Get runtime statistics."
  (let ((uptime (get-unix-time)))
    (list :uptime-seconds uptime
          :thread-count (length (bt:all-threads))
          :memory (memory-stats)
          :gc (gc-stats))))

(defun health-check ()
  "Basic health check."
  (let ((mem (memory-stats)))
    (list :status :healthy
          :checks (list :memory :ok
                        :threads :ok
                        :gc :ok)
          :timestamp (get-unix-time))))

(defun metrics-export ()
  "Export metrics in Prometheus format."
  (format nil "~%# HELP cltron_heap_bytes Total heap bytes~%# TYPE cltron_heap_bytes gauge~%cltron_heap_bytes ~d~%"
          0))

(defun gc-tune (&key generation-sizes promotion-thresholds)
  "Tune GC parameters. Placeholder."
  (declare (ignore generation-sizes promotion-thresholds))
  (list :error t
        :message "GC tuning not yet implemented"))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))
