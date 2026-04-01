;;;; src/core/request-tracing.lisp
;;;;
;;;; Request tracing with correlation IDs for the MCP protocol lifecycle.
;;;;
;;;; Each JSON-RPC request gets a unique trace ID that flows through:
;;;;   receive → validate → dispatch → tool-execute → respond
;;;;
;;;; Trace data is stored in a bounded ring buffer (default 1000 entries).
;;;; The current trace ID is stored in a dynamic variable so it propagates
;;;; automatically through the call stack without explicit threading.

(in-package :cl-tron-mcp/core)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *tracing-enabled* nil
  "Set to T to enable request tracing. Off by default to avoid overhead.")

(defvar *trace-buffer-size* 1000
  "Maximum number of completed trace entries to retain.")

;;; ============================================================
;;; Per-request dynamic context
;;; ============================================================

(defvar *current-trace-id* nil
  "Dynamic variable: the trace ID for the currently executing request.
Automatically inherited by any threads spawned during request handling
when using BT:MAKE-THREAD with dynamic binding.")

(defvar *current-trace-start* nil
  "Internal-real-time value when the current request started.")

;;; ============================================================
;;; Trace log (ring buffer)
;;; ============================================================

(defvar *trace-log-lock* (bordeaux-threads:make-lock "trace-log-lock"))

(defvar *trace-log* (make-array 1000 :initial-element nil)
  "Ring buffer of completed trace entries.")

(defvar *trace-log-index* 0
  "Next write position in *trace-log*.")

(defvar *trace-log-count* 0
  "Total number of traces ever written (for computing fill level).")

(defun %trace-write (entry)
  "Append ENTRY to the ring buffer. Called with lock held."
  (setf (aref *trace-log* *trace-log-index*) entry)
  (setf *trace-log-index* (mod (1+ *trace-log-index*) *trace-buffer-size*))
  (incf *trace-log-count*))

;;; ============================================================
;;; Trace ID generation
;;; ============================================================

(defvar *trace-counter* 0)
(defvar *trace-counter-lock* (bordeaux-threads:make-lock "trace-counter"))

(defun generate-trace-id ()
  "Return a unique trace ID string like \"tr-0001-<timestamp>\"."
  (let ((n (bordeaux-threads:with-lock-held (*trace-counter-lock*)
             (incf *trace-counter*))))
    (format nil "tr-~4,'0d-~d" n (get-universal-time))))

;;; ============================================================
;;; Public API
;;; ============================================================

(defmacro with-request-trace ((method &key request-id) &body body)
  "Establish a trace context for one JSON-RPC request.
Sets *current-trace-id* for the duration of BODY, records a completed
trace entry (including elapsed time and whether an error occurred) when
BODY exits.

  METHOD     — string, the JSON-RPC method name (e.g. \"tools/call\")
  REQUEST-ID — the JSON-RPC id field (may be nil for notifications)"
  (let ((trace-id (gensym "TRACE-ID-"))
        (start    (gensym "START-"))
        (err      (gensym "ERR-"))
        (result   (gensym "RESULT-")))
    `(if (not *tracing-enabled*)
         (progn ,@body)
         (let* ((,trace-id (generate-trace-id))
                (,start    (get-internal-real-time))
                (,err      nil)
                ,result)
           (let ((*current-trace-id*    ,trace-id)
                 (*current-trace-start* ,start))
             (unwind-protect
                  (handler-case
                      (setf ,result (progn ,@body))
                    (error (e)
                      (setf ,err (princ-to-string e))
                      (error e)))
               ;; Always record the completed trace
               (let ((elapsed-ms (round (* 1000
                                           (/ (- (get-internal-real-time) ,start)
                                              internal-time-units-per-second)))))
                 (bordeaux-threads:with-lock-held (*trace-log-lock*)
                   (%trace-write
                    (list :trace-id    ,trace-id
                          :method      ,method
                          :request-id  ,request-id
                          :elapsed-ms  elapsed-ms
                          :error       ,err
                          :timestamp   (get-universal-time)))))))
           ,result))))

(defun trace-log (message &rest args)
  "Emit a structured trace event associated with the current trace ID.
Does nothing if tracing is disabled or no trace is active."
  (when (and *tracing-enabled* *current-trace-id*)
    (let ((text (if args (apply #'format nil message args) message)))
      (bordeaux-threads:with-lock-held (*trace-log-lock*)
        (%trace-write
         (list :trace-id   *current-trace-id*
               :event      text
               :timestamp  (get-universal-time)))))))

(defun current-trace-id ()
  "Return the trace ID for the current request, or NIL."
  *current-trace-id*)

(defun get-trace-log (&key (limit 100))
  "Return up to LIMIT recent trace entries, newest first."
  (bordeaux-threads:with-lock-held (*trace-log-lock*)
    (let* ((total (min *trace-log-count* *trace-buffer-size*))
           (n     (min limit total))
           result)
      ;; Walk backwards from last written position
      (dotimes (i n)
        (let* ((pos (mod (- *trace-log-index* 1 i) *trace-buffer-size*))
               (entry (aref *trace-log* pos)))
          (when entry (push entry result))))
      result)))

(defun clear-trace-log ()
  "Erase all trace entries and reset the counter."
  (bordeaux-threads:with-lock-held (*trace-log-lock*)
    (fill *trace-log* nil)
    (setf *trace-log-index* 0
          *trace-log-count* 0)
    t))
