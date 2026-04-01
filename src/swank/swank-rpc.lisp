;;;; src/swank/swank-rpc.lisp - Request-response correlation, reader loop, dispatch
;;;;
;;;; Handles:
;;;;   - Request ID generation and the swank-request struct
;;;;   - Synchronous (wait-for-response) and asynchronous (send-request-async) RPC
;;;;   - Background reader thread (swank-reader-loop)
;;;;   - Heartbeat/keepalive thread (heartbeat-loop)
;;;;   - Incoming message dispatch (dispatch-incoming-message)
;;;;   - Output handling (handle-output)
;;;;
;;;; Load order: loaded after swank-connection.lisp

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Request-Response Correlation
;;; ============================================================
;;;
;;; Swank uses RPC with request IDs.
;;; Flow:
;;;   1. Client sends  (:emacs-rex form package thread id)
;;;   2. Server replies (:return (:ok result) id) or (:return (:abort) id)
;;;   3. Server may also send :debug, :write-string async events

(defun make-request-id ()
  (bordeaux-threads:with-lock-held (*request-lock*)
    (prog1 *next-request-id*
      (incf *next-request-id*))))

(defstruct swank-request
  id
  condition
  response
  completed-p)

(defun fulfill-request (id response)
  "Mark request ID as completed with RESPONSE."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (gethash id *pending-requests*)))
      (when req
        (setf (swank-request-response req) response
              (swank-request-completed-p req) t)
        (bordeaux-threads:condition-notify (swank-request-condition req))))))

(defun wait-for-response (id &key (timeout *default-eval-timeout*))
  "Wait for response to request ID with optional TIMEOUT (seconds)."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (gethash id *pending-requests*)))
      (unless req
        (return-from wait-for-response
          (list :error t :message (format nil "Request ~a not found" id))))
      (let ((start (get-unix-time)))
        (loop while (not (swank-request-completed-p req))
              for elapsed = (- (get-unix-time) start)
              when (> elapsed timeout)
                do (return-from wait-for-response
                     (list :error t :message "Request timeout"))
              do (bordeaux-threads:condition-wait
                  (swank-request-condition req) *request-lock*)
              finally (return
                        (if (swank-request-completed-p req)
                            (swank-request-response req)
                            (list :error t :message "Request timeout"))))))))

;;; ============================================================
;;; Reader Thread & Message Dispatch
;;; ============================================================

(defun swank-reader-loop ()
  "Background thread: continuously read incoming Swank messages."
  (log-debug "Swank reader thread started")
  (loop while *swank-running*
        do (handler-case
               (let* ((raw-message (cl-tron-mcp/swank-protocol:read-packet *swank-io*))
                      (message (handler-case
                                   (cl-tron-mcp/swank-protocol:read-form raw-message *swank-io-package*)
                                 (error (e)
                                   (log-error (format nil "Failed to parse message ~S: ~a" raw-message e))
                                   (return)))))
                 (log-debug (format nil "Received: ~s" message))
                 (bordeaux-threads:with-lock-held (*connection-lock*)
                   (setf *last-activity-time* (get-unix-time)))
                 (dispatch-incoming-message message))
             (error (e)
               (log-error (format nil "Swank reader error: ~a" e))
               (return)))
        finally (log-debug "Swank reader thread exiting")))

;;; ============================================================
;;; Heartbeat / Keepalive
;;; ============================================================

(defun heartbeat-loop ()
  "Background thread: sends periodic pings to detect dead connections."
  (log-debug "Swank heartbeat thread started")
  (loop while *heartbeat-running*
        do (sleep *heartbeat-interval*)
           (when (swank-connected-p)
             (handler-case
                 (let ((last-activity (bordeaux-threads:with-lock-held (*connection-lock*)
                                        *last-activity-time*)))
                   (when last-activity
                     (let ((elapsed (- (get-unix-time) last-activity)))
                       (when (> elapsed (* *heartbeat-interval* 2))
                         (log-warn (format nil "No activity from Swank for ~d seconds, attempting reconnection" elapsed))
                         (when *reconnect-enabled*
                           (attempt-reconnect))))))
               (error (e)
                 (log-error (format nil "Heartbeat error: ~a" e))))))
  (log-debug "Swank heartbeat thread exiting"))

;;; ============================================================
;;; Incoming Message Dispatch
;;; ============================================================

(defun dispatch-incoming-message (message)
  "Route an incoming Swank message to the appropriate handler."
  (when message
    (destructuring-bind (tag &rest args) message
      (case tag
        (:return
          (destructuring-bind (result id) args
            (fulfill-request id (list :result result))))
        (:debug
         (destructuring-bind (thread level condition restarts frames &optional extra) args
           (declare (ignore extra))
           (setf *debugger-thread* thread
                 *debugger-level* level)
           ;; enqueue-debugger-event is defined in swank-events.lisp (loaded after)
           (enqueue-debugger-event condition restarts frames)
           (bordeaux-threads:with-lock-held (*request-lock*)
             (when *current-request-id*
               (let ((req (gethash *current-request-id* *pending-requests*)))
                 (when req
                   (setf (swank-request-response req)
                         (list :result (list :debug t
                                             :thread thread
                                             :level level
                                             :condition condition
                                             :restarts restarts
                                             :frames frames))
                         (swank-request-completed-p req) t)
                   (bordeaux-threads:condition-notify (swank-request-condition req))))))))
        (:write-string
         (destructuring-bind (string &optional target thread-id) args
           (declare (ignore thread-id))
           (handle-output string target)))
        (:read-string
         ;; Swank is requesting input from the user.  We store the pending
         ;; request so that the caller can supply input via swank-provide-input.
         (destructuring-bind (thread-id tag) args
           (log-info (format nil "Swank requesting input (thread ~a tag ~a). Use swank_send_input to respond." thread-id tag))
           (bordeaux-threads:with-lock-held (*input-request-lock*)
             (push (cons thread-id tag) *pending-input-requests*))))
        (:debug-activate
         (destructuring-bind (thread-id level selections) args
           (declare (ignore selections))
           (setf *debugger-thread* thread-id
                 *debugger-level* level)))
        (:debug-return
         (destructuring-bind (thread-id level stepping-p) args
           (declare (ignore thread-id stepping-p))
           (when (<= level 0)
             (setf *debugger-thread* nil
                   *debugger-level* 0))))
        (:new-package
         (destructuring-bind (name prompt-string) args
           (declare (ignore prompt-string))
           (log-info (format nil "Swank package changed to ~a" name))))
        (:new-features
         ;; Sent after compile/load — ignore
         )
        (:indentation-update
         ;; Sent after compile — ignore
         )
        (:ping
         (destructuring-bind (thread-id tag) args
           (declare (ignore thread-id))
           (write-message `(:emacs-pong ,tag))))
        (t
         (log-warn (format nil "Unhandled Swank message: ~s" message)))))))

;;; ============================================================
;;; Request Sending
;;; ============================================================

(defun send-request (form &key (package "CL-USER") (thread t) (timeout *default-eval-timeout*))
  "Send :emacs-rex request and wait synchronously for the response.
FORM     — S-expression to evaluate.
PACKAGE  — package name string.
THREAD   — which thread to use (t, :repl-thread, or integer).
TIMEOUT  — maximum seconds to wait (default: *default-eval-timeout*)."
  (unless (swank-connected-p)
    (return-from send-request
      (cl-tron-mcp/core:make-error "SWANK_NOT_CONNECTED")))
  (let* ((id (make-request-id))
         (req (make-swank-request :id id
                                  :condition (bordeaux-threads:make-condition-variable)
                                  :response nil
                                  :completed-p nil)))
    (bordeaux-threads:with-lock-held (*request-lock*)
      (setf (gethash id *pending-requests*) req
            *current-request-id* id))
    (handler-case
        (progn
          (write-message `(:emacs-rex ,form ,package ,thread ,id))
          (wait-for-response id :timeout timeout))
      (error (e)
        (bordeaux-threads:with-lock-held (*request-lock*)
          (remhash id *pending-requests*)
          (when (eql *current-request-id* id)
            (setf *current-request-id* nil)))
        (cl-tron-mcp/core:make-error "INTERNAL_ERROR"
                                     :details (list :error (princ-to-string e)))))))

(defun handle-output (string target)
  "Handle :write-string output from Swank."
  (declare (ignore target))
  (when *output-callback*
    (handler-case
        (funcall *output-callback* string target)
      (error (e)
        (log-error (format nil "Output callback error: ~a" e)))))
  (enqueue-output-event string target))

;;; ============================================================
;;; Asynchronous Evaluation
;;; ============================================================

(defun send-request-async (form &key (package "CL-USER") (thread t))
  "Send :emacs-rex request asynchronously; return request ID immediately.
Use get-async-result with the returned ID to retrieve the result later."
  (unless (swank-connected-p)
    (return-from send-request-async
      (cl-tron-mcp/core:make-error "SWANK_NOT_CONNECTED")))
  (let* ((id (make-request-id))
         (req (make-swank-request :id id
                                  :condition (bordeaux-threads:make-condition-variable)
                                  :response nil
                                  :completed-p nil)))
    (bordeaux-threads:with-lock-held (*request-lock*)
      (setf (gethash id *pending-requests*) req
            *current-request-id* id))
    (handler-case
        (progn
          (write-message `(:emacs-rex ,form ,package ,thread ,id))
          id)
      (error (e)
        (bordeaux-threads:with-lock-held (*request-lock*)
          (remhash id *pending-requests*)
          (when (eql *current-request-id* id)
            (setf *current-request-id* nil)))
        (cl-tron-mcp/core:make-error "INTERNAL_ERROR"
                                     :details (list :error (princ-to-string e)))))))

(defun get-async-result (id &key (timeout *default-eval-timeout*))
  "Get the result of an async request by ID, waiting up to TIMEOUT seconds."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (gethash id *pending-requests*)))
      (unless req
        (return-from get-async-result
          (cl-tron-mcp/core:make-error "REQUEST_NOT_FOUND"
                                       :details (list :request-id id))))
      (if (swank-request-completed-p req)
          (progn
            (remhash id *pending-requests*)
            (swank-request-response req))
          (progn
            (bordeaux-threads:release-lock *request-lock*)
            (let ((result (wait-for-response id :timeout timeout)))
              (bordeaux-threads:acquire-lock *request-lock*)
              (remhash id *pending-requests*)
              result))))))
