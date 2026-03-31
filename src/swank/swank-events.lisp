;;;; src/swank/swank-events.lisp - Event queue, reconnection, event processor
;;;;
;;;; Handles:
;;;;   - swank-event struct (type, data, timestamp)
;;;;   - Enqueueing/dequeuing :debug and :output events
;;;;   - Automatic reconnection with exponential backoff
;;;;   - Background event processor thread
;;;;
;;;; Load order: loaded after swank-connection.lisp and swank-rpc.lisp

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Event Queue
;;; ============================================================
;;;
;;; Async events from Swank are queued for later processing:
;;;   - :debug  — debugger entered; retrieved by pop-debugger-event
;;;   - :output — text written by evaluated code

(defstruct swank-event
  type
  data
  timestamp)

(defun enqueue-debugger-event (condition restarts frames)
  "Enqueue a :debug event from Swank."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (when (>= (length *event-queue*) *max-event-queue-size*)
      (cleanup-old-events))
    (vector-push-extend
     (make-swank-event :type :debug
                       :data (list :condition condition
                                   :restarts restarts
                                   :frames frames)
                       :timestamp (get-unix-time))
     *event-queue*)
    (bordeaux-threads:condition-notify *event-condition*)))

(defun enqueue-output-event (string target)
  "Enqueue an :output event."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (when (>= (length *event-queue*) *max-event-queue-size*)
      (cleanup-old-events))
    (vector-push-extend
     (make-swank-event :type :output
                       :data (list :string string :target target)
                       :timestamp (get-unix-time))
     *event-queue*)
    (bordeaux-threads:condition-notify *event-condition*)))

(defun cleanup-old-events (&optional (max-age 300))
  "Remove events older than MAX-AGE seconds; also trim if queue is oversized."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (let ((current-time (get-unix-time))
          (new-queue (make-array 100 :adjustable t :fill-pointer 0)))
      (loop for event across *event-queue*
            for event-age = (- current-time (swank-event-timestamp event))
            unless (> event-age max-age)
              do (vector-push-extend event new-queue))
      (when (> (length new-queue) *max-event-queue-size*)
        (let ((start (max 0 (- (length new-queue) *max-event-queue-size*))))
          (setf new-queue (subseq new-queue start))))
      (setf *event-queue* new-queue))))

(defun dequeue-event (&optional (timeout 0.1))
  "Dequeue and return the next non-debug event, or NIL on timeout.
Debug events remain in the queue for pop-debugger-event."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (let ((start-time (get-unix-time)))
      (loop while (and (zerop (length *event-queue*))
                       *event-processor-running*)
            do (let ((elapsed (- (get-unix-time) start-time)))
                 (when (> elapsed timeout)
                   (return-from dequeue-event nil))
                 (bordeaux-threads:condition-wait
                  *event-condition* *event-mutex*
                  :timeout (- timeout elapsed)))))
    (loop for i from 0 below (length *event-queue*)
          for event = (aref *event-queue* i)
          unless (eq (swank-event-type event) :debug)
          do (setf *event-queue* (delete event *event-queue* :test #'eq))
             (return event))))

;;; ============================================================
;;; Reconnection (circuit breaker with exponential backoff)
;;; ============================================================

(defun attempt-reconnect (&key (host "127.0.0.1") (port 4006))
  "Attempt to reconnect to Swank with exponential backoff.
Returns connection status plist or error plist."
  (unless *reconnect-enabled*
    (return-from attempt-reconnect
      (cl-tron-mcp/core:make-error "RECONNECTION_DISABLED")))
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (when *swank-connected*
      (return-from attempt-reconnect
        (cl-tron-mcp/core:make-error "SWANK_ALREADY_CONNECTED"))))
  (let ((attempt-count (bordeaux-threads:with-lock-held (*connection-lock*)
                         (incf *reconnect-attempt-count*))))
    (when (> attempt-count *reconnect-max-attempts*)
      (log-error (format nil "Max reconnection attempts (~d) reached" *reconnect-max-attempts*))
      (bordeaux-threads:with-lock-held (*connection-lock*)
        (setf *reconnect-attempt-count* 0))
      (return-from attempt-reconnect
        (cl-tron-mcp/core:make-error "MAX_RECONNECTION_ATTEMPTS"
                                     :details (list :max-attempts *reconnect-max-attempts*))))
    (let ((delay (* *reconnect-delay* (expt 2 (1- attempt-count)))))
      (log-info (format nil "Reconnection attempt ~d/~d, waiting ~d seconds"
                        attempt-count *reconnect-max-attempts* delay))
      (sleep delay))
    (handler-case
        (let ((result (swank-connect :host host :port port)))
          (if (getf result :error)
              (progn
                (log-error (format nil "Reconnection attempt ~d failed: ~a"
                                  attempt-count (getf result :message)))
                result)
              (progn
                (log-info (format nil "Reconnection successful on attempt ~d" attempt-count))
                (bordeaux-threads:with-lock-held (*connection-lock*)
                  (setf *reconnect-attempt-count* 0))
                result)))
      (error (e)
        (log-error (format nil "Reconnection attempt ~d error: ~a" attempt-count e))
        (cl-tron-mcp/core:make-error "RECONNECTION_ERROR"
                                     :details (list :error (princ-to-string e)))))))

(defun cleanup-on-error ()
  "Clean up resources on fatal errors: disconnect, clear pending state."
  (handler-case
      (progn
        (log-warn "Cleaning up after fatal error")
        (swank-disconnect)
        (bordeaux-threads:with-lock-held (*request-lock*)
          (clrhash *pending-requests*)
          (setf *current-request-id* nil))
        (bordeaux-threads:with-lock-held (*event-mutex*)
          (setf (fill-pointer *event-queue*) 0))
        (setf *debugger-thread* nil
              *debugger-level* 0)
        (log-info "Cleanup completed"))
    (error (e)
      (log-error (format nil "Error during cleanup: ~a" e)))))

;;; ============================================================
;;; Event Processor Thread
;;; ============================================================

(defun swank-event-processor ()
  "Background thread: consumes non-debug events from the queue."
  (log-debug "Swank event processor started")
  (loop while *event-processor-running*
        do (let ((event (dequeue-event 1)))
             (when event
               (handle-swank-event event))))
  (log-debug "Swank event processor exiting"))

(defun handle-swank-event (event)
  "Process a single Swank event.
:debug events remain in the queue for pop-debugger-event; :output events are logged."
  (ecase (swank-event-type event)
    (:debug
     (log-info (format nil "Debugger event queued: ~a"
                       (getf (swank-event-data event) :condition))))
    (:output
     (let ((data (swank-event-data event)))
       (log-debug (format nil "Output: ~a" (getf data :string)))))))

;;; ============================================================
;;; Debugger Event API (used by MCP tools)
;;; ============================================================

(defun pop-debugger-event ()
  "Pop and return the most recent debug event as (values condition restarts frames).
Returns NIL if no debug event is queued."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (loop for i from (1- (length *event-queue*)) downto 0
          when (eq (swank-event-type (aref *event-queue* i)) :debug)
          do (let* ((event (aref *event-queue* i))
                    (data (swank-event-data event)))
               (setf *event-queue*
                     (delete (aref *event-queue* i) *event-queue* :test #'eq))
               (return (values (getf data :condition)
                               (getf data :restarts)
                               (getf data :frames)))))))
