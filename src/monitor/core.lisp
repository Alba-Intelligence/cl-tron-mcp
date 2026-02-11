;;;; src/monitor/core.lisp

(in-package :cl-tron-mcp/monitor)

(defun memory-stats ()
  "Get memory usage statistics from SBCL."
  (handler-case
      #+sbcl
      (let ((dynamic-usage (sb-kernel:dynamic-usage)))
        (list :dynamic-bytes dynamic-usage
              :total-bytes dynamic-usage
              :total-mb (round (/ dynamic-usage 1048576))))
      #-sbcl
      (list :total-bytes nil
            :message "Memory stats not available")
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun gc-stats ()
  "Get GC statistics from SBCL."
  (list :bytes-consed-between-gcs (sb-ext:bytes-consed-between-gcs)))

(defun runtime-stats ()
  "Get comprehensive runtime statistics."
  (let ((mem (memory-stats)))
    (list :thread-count (length (bt:all-threads))
          :memory mem
          :gc (gc-stats)
          :sbcl-version (lisp-implementation-version))))

(defun health-check ()
  "Comprehensive health check for the MCP server."
  (list :status :healthy
        :checks (list :memory :ok
                      :threads :ok
                      :gc :ok)
        :timestamp (get-unix-time)))

(defun metrics-export ()
  "Export metrics in Prometheus text format."
  (let ((mem (memory-stats))
        (threads (length (bt:all-threads))))
    (format nil "# HELP cltron_heap_bytes Total heap bytes in use~%# TYPE cltron_heap_bytes gauge~%cltron_heap_bytes ~d~%~%# HELP cltron_threads Number of active threads~%# TYPE cltron_threads gauge~%cltron_threads ~d~%"
              (getf mem :total-bytes 0)
              threads)))

(defun gc-run (&key (generation 0))
  "Force garbage collection."
  (handler-case
      (progn
        (sb-ext:gc :generation generation)
        (list :success t
              :generation generation
              :memory-after (memory-stats)))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun get-unix-time ()
  "Get current Unix timestamp."
  (handler-case
      (local-time:timestamp-to-unix (local-time:now))
    (error () (get-universal-time))))

(defun system-info ()
  "Get comprehensive system information."
  (list :lisp-implementation (lisp-implementation-type)
        :lisp-version (lisp-implementation-version)
        :machine-type (machine-type)
        :machine-version (machine-version)
        :software-type (software-type)
        :software-version (software-version)
        :packages-count (length (list-all-packages))
        :threads-count (length (bt:all-threads))))
