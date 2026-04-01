;;;; src/core/metrics.lisp
;;;;
;;;; Request metrics collection: latency, error rates, and tool usage stats.
;;;; Provides lightweight counters and histograms for production monitoring.
;;;;
;;;; Thread-safe via a single lock. Designed for low overhead — only atomic
;;;; increments and time measurements; no heap allocation per request.

(in-package :cl-tron-mcp/core)

;;; ============================================================
;;; State
;;; ============================================================

(defvar *metrics-enabled* t
  "Set to NIL to disable all metrics collection.")

(defvar *metrics-lock* (bordeaux-threads:make-lock "metrics-lock")
  "Global lock protecting all *metrics-* variables.")

(defvar *metrics-tool-calls* (make-hash-table :test 'equal)
  "Hash: tool-name -> total call count.")

(defvar *metrics-tool-errors* (make-hash-table :test 'equal)
  "Hash: tool-name -> total error count.")

(defvar *metrics-tool-latency-sum* (make-hash-table :test 'equal)
  "Hash: tool-name -> sum of latency in milliseconds (for computing mean).")

(defvar *metrics-tool-latency-count* (make-hash-table :test 'equal)
  "Hash: tool-name -> number of latency samples recorded.")

(defvar *metrics-request-count* 0
  "Total JSON-RPC requests received since last reset.")

(defvar *metrics-error-count* 0
  "Total JSON-RPC requests that resulted in errors since last reset.")

(defvar *metrics-start-time* (get-universal-time)
  "Unix timestamp when metrics collection started (or last reset).")

;;; ============================================================
;;; Recording API
;;; ============================================================

(defun metrics-record-call (tool-name latency-ms &key (error-p nil))
  "Record one tool invocation. LATENCY-MS is the wall-clock time in milliseconds.
ERROR-P should be T if the invocation resulted in an error."
  (when *metrics-enabled*
    (bordeaux-threads:with-lock-held (*metrics-lock*)
      (incf *metrics-request-count*)
      (when error-p (incf *metrics-error-count*))
      ;; Per-tool call count
      (setf (gethash tool-name *metrics-tool-calls*)
            (1+ (gethash tool-name *metrics-tool-calls* 0)))
      ;; Per-tool error count
      (when error-p
        (setf (gethash tool-name *metrics-tool-errors*)
              (1+ (gethash tool-name *metrics-tool-errors* 0))))
      ;; Latency accumulation
      (when latency-ms
        (setf (gethash tool-name *metrics-tool-latency-sum*)
              (+ (gethash tool-name *metrics-tool-latency-sum* 0) latency-ms))
        (setf (gethash tool-name *metrics-tool-latency-count*)
              (1+ (gethash tool-name *metrics-tool-latency-count* 0)))))))

(defmacro with-metrics (tool-name &body body)
  "Execute BODY measuring wall-clock latency and recording metrics for TOOL-NAME.
Automatically marks the call as an error if BODY signals a condition."
  (let ((start (gensym "START-"))
        (error-p (gensym "ERROR-P-"))
        (name (gensym "NAME-")))
    `(let ((,start (get-internal-real-time))
           (,error-p nil)
           (,name ,tool-name))
       (unwind-protect
            (handler-case (progn ,@body)
              (error (e)
                (setf ,error-p t)
                (error e)))
         (let ((latency-ms (round (* 1000 (/ (- (get-internal-real-time) ,start)
                                             internal-time-units-per-second)))))
           (metrics-record-call ,name latency-ms :error-p ,error-p))))))

;;; ============================================================
;;; Query API
;;; ============================================================

(defun metrics-snapshot ()
  "Return an immutable snapshot of all current metrics as a plist.
Safe to call from any thread."
  (bordeaux-threads:with-lock-held (*metrics-lock*)
    (let ((uptime (- (get-universal-time) *metrics-start-time*))
          tool-stats)
      ;; Build per-tool stats list
      (maphash
       (lambda (name calls)
         (let* ((errors (gethash name *metrics-tool-errors* 0))
                (lat-sum (gethash name *metrics-tool-latency-sum* 0))
                (lat-n   (gethash name *metrics-tool-latency-count* 0))
                (avg-ms  (if (> lat-n 0)
                             (round (/ lat-sum lat-n))
                             0))
                (err-rate (if (> calls 0)
                              (float (/ errors calls))
                              0.0)))
           (push (list :tool name
                       :calls calls
                       :errors errors
                       :error-rate err-rate
                       :avg-latency-ms avg-ms)
                 tool-stats)))
       *metrics-tool-calls*)
      (list :uptime-seconds uptime
            :total-requests *metrics-request-count*
            :total-errors *metrics-error-count*
            :overall-error-rate (if (> *metrics-request-count* 0)
                                    (float (/ *metrics-error-count*
                                              *metrics-request-count*))
                                    0.0)
            :tools (sort tool-stats #'> :key (lambda (x) (getf x :calls)))))))

(defun metrics-reset ()
  "Clear all metrics and restart the uptime clock."
  (bordeaux-threads:with-lock-held (*metrics-lock*)
    (clrhash *metrics-tool-calls*)
    (clrhash *metrics-tool-errors*)
    (clrhash *metrics-tool-latency-sum*)
    (clrhash *metrics-tool-latency-count*)
    (setf *metrics-request-count* 0
          *metrics-error-count* 0
          *metrics-start-time* (get-universal-time))
    t))

(defun metrics-for-tool (tool-name)
  "Return metrics for a single tool as a plist, or NIL if no data."
  (bordeaux-threads:with-lock-held (*metrics-lock*)
    (let ((calls (gethash tool-name *metrics-tool-calls*)))
      (when calls
        (let* ((errors  (gethash tool-name *metrics-tool-errors* 0))
               (lat-sum (gethash tool-name *metrics-tool-latency-sum* 0))
               (lat-n   (gethash tool-name *metrics-tool-latency-count* 0)))
          (list :tool tool-name
                :calls calls
                :errors errors
                :error-rate (if (> calls 0) (float (/ errors calls)) 0.0)
                :avg-latency-ms (if (> lat-n 0) (round (/ lat-sum lat-n)) 0)))))))
