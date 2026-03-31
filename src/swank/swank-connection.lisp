;;;; src/swank/swank-connection.lisp - Swank connection management
;;;;
;;;; This file handles the low-level connection to a running SBCL+Swank
;;;; session: socket setup, connection state variables, connect/disconnect,
;;;; and the raw protocol I/O functions (read-packet, write-message).
;;;;
;;;; SWANK package placeholder:
;;;;   The MCP process does NOT load Swank locally — it only serialises
;;;;   symbols like SWANK:EVAL-AND-GRAB-OUTPUT that the remote Swank server
;;;;   will resolve.  We create a minimal placeholder :swank package for this.
;;;;
;;;; Load order within src/swank/:
;;;;   swank-connection  ← this file
;;;;   swank-rpc         (request-response correlation, reader loop)
;;;;   swank-events      (event queue, reconnect)
;;;;   swank-api         (high-level RPC operations, MCP wrappers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :bordeaux-threads :silent t))

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; SWANK package placeholder
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :swank)
    (make-package :swank :use '())))

;;; ============================================================
;;; I/O package for reading Swank responses
;;; ============================================================
;;;
;;; Only imports NIL, T and QUOTE so arbitrary symbols in responses
;;; don't intern into well-known packages.

(defvar *swank-io-package* (find-package :swank-io-package))
(unless *swank-io-package*
  (setf *swank-io-package*
        (make-package :swank-io-package :use '())))

(defun swank-sym (name)
  "Create a symbol in the SWANK placeholder package for protocol messages.
Example: (swank-sym \"EVAL-AND-GRAB-OUTPUT\") => SWANK:EVAL-AND-GRAB-OUTPUT"
  (declare (type string name))
  (intern (string-upcase name) :swank))

;;; ============================================================
;;; Utility
;;; ============================================================

(defun get-unix-time ()
  "Return current Unix timestamp as an integer."
  #+sbcl  (sb-ext:get-time-of-day)
  #+ccl   (ccl:get-time-of-day)
  #-(or sbcl ccl) (- (/ (get-universal-time) 86400) 2))

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *swank-socket* nil
  "Socket connection to Swank server.")

(defvar *swank-io* nil
  "Bidirectional I/O stream for Swank communication.")

(defvar *swank-connected* nil
  "Connection flag — T when connected to a Swank server.")

(defvar *swank-reader-thread* nil
  "Background thread that reads incoming Swank messages.")

(defvar *swank-running* nil
  "Control flag for reader thread — set to NIL to stop.")

(defvar *connection-lock* (bordeaux-threads:make-lock "swank-connection")
  "Lock for synchronising access to connection state variables.")

;;; Request tracking (for synchronous RPC calls)
(defvar *pending-requests* (make-hash-table :test 'eql)
  "Hash table mapping request IDs to swank-request structures.")
(defvar *request-lock* (bordeaux-threads:make-lock "swank-requests")
  "Lock for synchronising access to *pending-requests*.")
(defvar *current-request-id* nil
  "ID of the most recent request sent (used to match :debug events to requests).")
(defvar *next-request-id* 1
  "Counter for generating unique request IDs.")

;;; Event queue for asynchronous messages (:debug, :write-string, etc.)
(defvar *event-queue* (make-array 100 :adjustable t :fill-pointer 0)
  "Queue of async events received from Swank (:debug, :write-string, etc.).")
(defvar *event-mutex* (bordeaux-threads:make-lock "swank-events")
  "Lock for synchronising access to *event-queue*.")
(defvar *event-condition* (bordeaux-threads:make-condition-variable)
  "Condition variable for waiting on events.")
(defvar *event-processor-running* nil
  "Flag indicating if the event processor thread is running.")
(defvar *event-processor-thread* nil
  "Thread that processes async events.")
(defvar *max-event-queue-size* 1000
  "Maximum number of events to keep in the queue before dropping oldest.")

;;; Pending input requests (for :read-string Swank messages)
(defvar *pending-input-requests* nil
  "List of pending (thread-id . tag) pairs for :read-string requests.")
(defvar *input-request-lock* (bordeaux-threads:make-lock "swank-input")
  "Lock for synchronising access to *pending-input-requests*.")

;;; Debugger state
(defvar *debugger-thread* nil
  "Thread ID currently in the debugger, or NIL.")
(defvar *debugger-level* 0
  "Current debugger level (0 = not in debugger).")

;;; ============================================================
;;; Timeout and Heartbeat Configuration
;;; ============================================================

(defvar *default-eval-timeout* 30
  "Default timeout for evaluation requests in seconds.")

(defvar *heartbeat-interval* 60
  "Heartbeat interval in seconds for keepalive pings.")

(defvar *last-activity-time* nil
  "Timestamp of last activity from Swank server.")

(defvar *heartbeat-thread* nil
  "Thread that sends periodic heartbeat pings.")

(defvar *heartbeat-running* nil
  "Control flag for heartbeat thread.")

;;; ============================================================
;;; Reconnection Configuration
;;; ============================================================

(defvar *reconnect-enabled* t
  "Whether automatic reconnection is enabled.")

(defvar *reconnect-max-attempts* 5
  "Maximum number of reconnection attempts.")

(defvar *reconnect-delay* 5
  "Initial delay between reconnection attempts in seconds.")

(defvar *reconnect-attempt-count* 0
  "Current reconnection attempt counter.")

;;; ============================================================
;;; Output Streaming
;;; ============================================================

(defvar *output-callback* nil
  "Callback (string target) called when output is received from Swank.")

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun swank-connect (&key (host "127.0.0.1") (port 4006) (timeout 10))
  "Connect to a running SBCL with Swank loaded.
On the SBCL side: (ql:quickload :swank) (swank:create-server :port 4006)

Returns: Connection status plist or error."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (when (and *swank-connected* *swank-socket*)
      (return-from swank-connect
        (cl-tron-mcp/core:make-error "SWANK_ALREADY_CONNECTED")))
    (handler-case
        (let ((socket (usocket:socket-connect host port :timeout timeout
                                              :element-type '(unsigned-byte 8))))
          (setf *swank-socket* socket
                *swank-io* (usocket:socket-stream socket)
                *swank-connected* t
                *swank-running* t
                *last-activity-time* (get-unix-time)
                *reconnect-attempt-count* 0)
          ;; Start reader, event processor, and heartbeat threads.
          ;; These are defined in swank-rpc.lisp and swank-events.lisp,
          ;; which are loaded after this file — safe due to late binding.
          (setf *swank-reader-thread*
                (bordeaux-threads:make-thread
                 #'swank-reader-loop
                 :name "swank-reader"))
          (setf *event-processor-running* t
                *event-processor-thread*
                (bordeaux-threads:make-thread
                 #'swank-event-processor
                 :name "swank-event-processor"))
          (setf *heartbeat-running* t
                *heartbeat-thread*
                (bordeaux-threads:make-thread
                 #'heartbeat-loop
                 :name "swank-heartbeat"))
          (log-info (format nil "Connected to Swank at ~a:~a" host port))
          (list :success t
                :host host :port port
                :message (format nil "Connected to Swank at ~a:~a" host port)))
      (error (e)
        (cl-tron-mcp/core:make-error "SWANK_CONNECTION_FAILED"
                                     :details (list :error (princ-to-string e)))))))

(defun swank-disconnect ()
  "Disconnect from the Swank server and stop all background threads."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (setf *swank-running* nil
          *event-processor-running* nil
          *heartbeat-running* nil))
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (bordeaux-threads:condition-notify *event-condition*))
  (when *swank-io*
    (ignore-errors (close *swank-io* :abort t)))
  (when *swank-reader-thread*
    (ignore-errors (bordeaux-threads:join-thread *swank-reader-thread* :timeout 2)))
  (when *event-processor-thread*
    (ignore-errors (bordeaux-threads:join-thread *event-processor-thread* :timeout 2)))
  (when *heartbeat-thread*
    (ignore-errors (bordeaux-threads:join-thread *heartbeat-thread* :timeout 2)))
  (when *swank-socket*
    (ignore-errors (usocket:socket-close *swank-socket*)))
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (setf *swank-socket* nil
          *swank-io* nil
          *swank-connected* nil
          *swank-reader-thread* nil
          *event-processor-thread* nil
          *heartbeat-thread* nil
          *last-activity-time* nil))
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (setf (fill-pointer *event-queue*) 0))
  (log-info "Disconnected from Swank server")
  (list :success t :message "Disconnected from Swank server"))

(defun swank-connected-p ()
  "Return T if connected to Swank."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (and *swank-connected* *swank-socket* *swank-io*)))

(defun swank-status ()
  "Return current Swank connection status as a plist."
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (list :connected (and *swank-connected* *swank-socket* *swank-io*)
          :has-connection (not (null *swank-socket*))
          :reader-thread-alive (and *swank-reader-thread*
                                    (bordeaux-threads:thread-alive-p *swank-reader-thread*))
          :event-processor-alive (and *event-processor-thread*
                                      (bordeaux-threads:thread-alive-p *event-processor-thread*))
          :heartbeat-alive (and *heartbeat-thread*
                                 (bordeaux-threads:thread-alive-p *heartbeat-thread*))
          :last-activity *last-activity-time*)))

;;; ============================================================
;;; Message Protocol I/O
;;; ============================================================

(defun read-packet ()
  "Read and parse a single Swank packet from *swank-io*."
  (cl-tron-mcp/swank-protocol:read-message *swank-io* *swank-io-package*))

(defun write-message (message)
  "Write MESSAGE to Swank server using proper length-prefixed encoding."
  (cl-tron-mcp/swank-protocol:write-message message
                                            (or *swank-io-package* (find-package :cl))
                                            *swank-io*))
