;;;; src/swank/client.lisp - Proper Swank client for CL-TRON-MCP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket :silent t)
  (ql:quickload :bordeaux-threads :silent t))

(in-package #:cl-tron-mcp/swank)

;;; Helper to resolve Swank symbols at runtime
(defun swank-sym (name)
  "Find symbol NAME in the SWANK package at runtime.
Returns the symbol or signals an error if not found."
  (let ((pkg (find-package :swank)))
    (unless pkg
      (error "SWANK package not loaded. Connect to a Swank server first."))
    (let ((sym (find-symbol (string-upcase name) pkg)))
      (unless sym
        (error "Symbol ~a not found in SWANK package" name))
      sym)))

;;; Import logging functions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :cl-tron-mcp/logging)
    (shadowing-import '(cl-tron-mcp/logging:log-info
                        cl-tron-mcp/logging:log-debug
                        cl-tron-mcp/logging:log-warn
                        cl-tron-mcp/logging:log-error))))

;;; ============================================================
;;; Package-local Swank protocol package
;;; ============================================================

(defvar *swank-io-package* (find-package :swank-io-package))
(unless *swank-io-package*
  (setf *swank-io-package*
        (make-package :swank-io-package :use '())))

;;; ============================================================
;;; Connection State
;;; ============================================================

(defvar *swank-socket* nil
  "Socket connection to Swank server.")

(defvar *swank-io* nil
  "Bidirectional I/O stream for Swank communication.")

(defvar *swank-connected* nil
  "Connection flag.")

(defvar *swank-reader-thread* nil
  "Background thread that reads incoming Swank messages.")

(defvar *swank-running* nil
  "Control flag for reader thread.")

;;; Request tracking (for synchronous RPC calls)
(defvar *pending-requests* (make-hash-table :test 'eql))
(defvar *request-lock* (bordeaux-threads:make-lock "swank-requests"))
(defvar *next-request-id* 1)

;;; Event queue for asynchronous messages (:debug, :write-string, etc.)
(defvar *event-queue* (make-array 100 :adjustable t :fill-pointer 0))
(defvar *event-mutex* (bordeaux-threads:make-lock "swank-events"))
(defvar *event-condition* (bordeaux-threads:make-condition-variable))
(defvar *event-processor-running* nil)
(defvar *event-processor-thread* nil)

;;; ============================================================
;;; Connection Management
;;; ============================================================

(defun swank-connect (&key (host "127.0.0.1") (port 4005) (timeout 10))
  "Connect to a running SBCL with Swank loaded.
On the SBCL side: (ql:quickload :swank) (swank:create-server :port 4005)

Returns: Connection status or error."
  (when (and *swank-connected* *swank-socket*)
    (return-from swank-connect
      (list :error t :message "Already connected to Swank server")))
  (handler-case
      (let ((socket (usocket:socket-connect host port :timeout timeout
                                            :element-type '(unsigned-byte 8))))
        (setf *swank-socket* socket
              *swank-io* (usocket:socket-stream socket)
              *swank-connected* t
              *swank-running* t)
        ;; Start reader thread
        (setf *swank-reader-thread*
              (bordeaux-threads:make-thread
               #'swank-reader-loop
               :name "swank-reader"))
        ;; Start event processor thread
        (setf *event-processor-running* t
              *event-processor-thread*
              (bordeaux-threads:make-thread
               #'swank-event-processor
               :name "swank-event-processor"))
        (log-info (format nil "Connected to Swank at ~a:~a" host port))
        (list :success t
              :host host :port port
              :message (format nil "Connected to Swank at ~a:~a" host port)))
    (error (e)
      (list :error t :message (format nil "Failed to connect to Swank: ~a" e)))))

(defun swank-disconnect ()
  "Disconnect from the Swank server and stop all threads."
  (setf *swank-running* nil)
  (setf *event-processor-running* nil)
  ;; Wake up event processor
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (bordeaux-threads:condition-notify *event-condition*))
  ;; Stop reader thread by closing socket (will cause read error)
  (when *swank-io*
    (ignore-errors (close *swank-io* :abort t)))
  (when *swank-reader-thread*
    (ignore-errors (bordeaux-threads:join-thread *swank-reader-thread* :timeout 2)))
  ;; Stop event processor
  (when *event-processor-thread*
    (ignore-errors (bordeaux-threads:join-thread *event-processor-thread* :timeout 2)))
  ;; Close socket
  (when *swank-socket*
    (ignore-errors (usocket:socket-close *swank-socket*)))
  (setf *swank-socket* nil
        *swank-io* nil
        *swank-connected* nil
        *swank-reader-thread* nil
        *event-processor-thread* nil)
  (log-info "Disconnected from Swank server")
  (list :success t :message "Disconnected from Swank server"))

(defun swank-connected-p ()
  "Check if connected to Swank."
  (and *swank-connected* *swank-socket* *swank-io*))

(defun swank-status ()
  "Get current Swank connection status."
  (list :connected (swank-connected-p)
        :has-connection (not (null *swank-socket*))
        :reader-thread-alive (and *swank-reader-thread*
                                  (bordeaux-threads:thread-alive-p *swank-reader-thread*))
        :event-processor-alive (and *event-processor-thread*
                                    (bordeaux-threads:thread-alive-p *event-processor-thread*))))

;;; ============================================================
;;; Message Protocol (using cl-tron-mcp/swank-protocol)
;;; ============================================================

(defun read-packet ()
  "Read a single Swank packet from *swank-io*."
  (cl-tron-mcp/swank-protocol:read-packet *swank-io*))

(defun write-message (message)
  "Write MESSAGE to Swank server using proper protocol."
  (cl-tron-mcp/swank-protocol:write-message message
                                            (or *swank-io-package* (find-package :cl))
                                            *swank-io*))

;;; ============================================================
;;; Request-Response Correlation
;;; ============================================================

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

(defun wait-for-response (id &key (timeout 30))
  "Wait for response to request ID with optional TIMEOUT (seconds)."
  (bordeaux-threads:with-lock-held (*request-lock*)
    (let ((req (gethash id *pending-requests*)))
      (unless req
        (return-from wait-for-response (list :error t :message (format nil "Request ~a not found" id))))
      (let ((start (get-unix-time)))
        (loop while (not (swank-request-completed-p req))
              for elapsed = (- (get-unix-time) start)
              when (> elapsed timeout)
                do (return-from wait-for-response (list :error t :message "Request timeout"))
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
               (let ((message (read-packet)))
                 (log-debug "Received: ~s" message)
                 (dispatch-incoming-message message))
             (error (e)
               (log-error "Swank reader error: ~a" e)
               (return)))
        finally (log-debug "Swank reader thread exiting")))

(defun dispatch-incoming-message (message)
  "Route incoming Swank message to appropriate handler."
  (when message
    (destructuring-bind (tag &rest args) message
      (ecase tag
        (:return
         (destructuring-bind (thread result id) args
           (fulfill-request id (list :thread thread :result result))))
        (:debug
         (destructuring-bind (thread level condition restarts frames) args
           (enqueue-debugger-event condition restarts frames)))
        (:write-string
         (destructuring-bind (string &optional target thread-id) args
           (handle-output string target)))
        (:read-string
         (destructuring-bind (thread-id tag) args
           (declare (ignore thread-id tag))
           ;; TODO: Implement input request handling
           ))
        (:debug-activate
         (destructuring-bind (thread-id level selections) args
           (declare (ignore thread-id level selections))
           ;; Debugger activated
           ))
        (:debug-return
         (destructuring-bind (thread-id level stepping-p) args
           (declare (ignore thread-id level stepping-p))
           ;; Debugger exited
           ))
        (:new-package
         (destructuring-bind (name prompt-string) args
           (log-info "Swank package changed to ~a" name)))
        (:ping
         (destructuring-bind (thread-id tag) args
           (declare (ignore thread-id))
           (write-message `(:emacs-pong ,tag))))
        (t
         (log-warn "Unhandled Swank message: ~s" message))))))

;;; ============================================================
;;; Request Sending
;;; ============================================================

(defun send-request (form &key (package "CL-USER") (thread t))
  "Send :emacs-rex request and wait for response.
FORM is the S-expression to evaluate.
PACKAGE is the package name string.
THREAD indicates which thread to use (t, :repl-thread, or integer)."
  (unless (swank-connected-p)
    (return-from send-request
      (list :error t :message "Not connected to Swank server")))
  (let* ((id (make-request-id))
         (req (make-swank-request :id id
                                  :condition (bordeaux-threads:make-condition-variable)
                                  :response nil
                                  :completed-p nil)))
    (bordeaux-threads:with-lock-held (*request-lock*)
      (setf (gethash id *pending-requests*) req))
    (handler-case
        (progn
          (write-message `(:emacs-rex ,form ,package ,thread ,id))
          (wait-for-response id :timeout 30))
      (error (e)
        (bordeaux-threads:with-lock-held (*request-lock*)
          (remhash id *pending-requests*))
        (list :error t :message (princ-to-string e))))))

;;; ============================================================
;;; Event Queue (for async :debug, :write-string)
;;; ============================================================

(defstruct swank-event
  type
  data
  timestamp)

(defun enqueue-debugger-event (condition restarts frames)
  "Enqueue a debugger event from Swank."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (vector-push-extend
     (make-swank-event :type :debug
                       :data (list :condition condition
                                   :restarts restarts
                                   :frames frames)
                       :timestamp (get-unix-time))
     *event-queue*)
    (bordeaux-threads:condition-notify *event-condition*)))

(defun enqueue-output-event (string target)
  "Enqueue output event."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (vector-push-extend
     (make-swank-event :type :output
                       :data (list :string string :target target)
                       :timestamp (get-unix-time))
     *event-queue*)
    (bordeaux-threads:condition-notify *event-condition*)))

(defun dequeue-event (&optional (timeout 0.1))
  "Dequeue and return next event, or NIL if timeout."
  (declare (ignore timeout))
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (loop while (and (zerop (length *event-queue*))
                     *event-processor-running*)
          do (bordeaux-threads:condition-wait
               *event-condition* *event-mutex*))
    (unless (zerop (length *event-queue*))
      (vector-pop *event-queue*))))

(defun swank-event-processor ()
  "Background thread: processes async events (currently just consumes them).
Events could be forwarded to MCP clients as notifications in the future."
  (log-debug "Swank event processor started")
  (loop while *event-processor-running*
        do (let ((event (dequeue-event 1)))
             (when event
               (handle-swank-event event))))
  (log-debug "Swank event processor exiting"))

(defun handle-swank-event (event)
  "Process a Swank event (for now, just log)."
  (ecase (swank-event-type event)
    (:debug
     (log-info "Debugger event: ~a" (getf (swank-event-data event) :condition)))
    (:output
     (let ((data (swank-event-data event)))
       (log-debug "Output: ~a" (getf data :string))))))

;;; ============================================================
;;; RPC Operations (MCP Tool Handlers)
;;; ============================================================

(defun swank-eval (&key code (package "CL-USER"))
  "Evaluate Lisp code via Swank RPC.
Code should be a string. Package is package name."
  (unless code
    (return-from swank-eval (list :error t :message "code is required")))
  (let ((form (read-from-string code)))
    (send-request form :package package :thread t)))

(defun swank-compile (&key code (package "CL-USER") (filename "repl"))
  "Compile Lisp code via Swank."
  (unless code
    (return-from swank-compile (list :error t :message "code is required")))
  (let ((form `(,(swank-sym "COMPILE-STRING-FOR-EMACS") ,code
                                                 :buffer ,filename
                                                 :position 0
                                                 :filename ,filename
                                                 :policy nil)))
    (send-request form :package package :thread t)))

(defun swank-backtrace (&key (start 0) (end 20))
  "Get backtrace from Swank."
  (let ((form `(,(swank-sym "BACKTRACE") ,start ,end)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-frame-locals (&key (frame-index 0) (thread :repl-thread))
  "Get local variables for FRAME-INDEX."
  (let ((form `(,(swank-sym "FRAME-LOCALS-AND-CATCH-TAGS") ,frame-index)))
    (send-request form :package "CL-USER" :thread thread)))

(defun swank-eval-in-frame (&key code (frame-index 0) (package "CL-USER"))
  "Evaluate CODE (string) in FRAME-INDEX context."
  (unless code
    (return-from swank-eval-in-frame (list :error t :message "code is required")))
  (let ((form `(,(swank-sym "EVAL-STRING-IN-FRAME") ,code ,frame-index ,package)))
    (send-request form :package package :thread t)))

(defun swank-invoke-restart (&key (restart-index 1))
  "Invoke the Nth restart (1-based index)."
  (let ((form `(,(swank-sym "INVOKE-NTH-RESTART") ,restart-index)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-get-restarts (&optional (frame-index 0))
  "Get available restarts for FRAME-INDEX (default 0 = current/top frame)."
  (let ((form `(,(swank-sym "COMPUTE-RESTARTS-FOR-EMACS") ,frame-index)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-step (&key (frame-index 0))
  "Step into next expression."
  (let ((form `(,(swank-sym "SLDB-STEP-INTO") ,frame-index)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-next (&key (frame-index 0))
  "Step over next expression."
  (let ((form `(,(swank-sym "SLDB-STEP-NEXT") ,frame-index)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-out (&key (frame-index 0))
  "Step out of current frame."
  (let ((form `(,(swank-sym "SLDB-STEP-OUT") ,frame-index)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-continue ()
  "Continue execution from debugger."
  (send-request (swank-sym "SLDB-CONTINUE") :package "CL-USER" :thread t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Breakpoint RPCs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun swank-set-breakpoint (&key function condition hit-count thread)
  "Set a breakpoint via Swank."
  (declare (ignore condition hit-count thread))
  (let ((form `(,(swank-sym "BREAK") ,function)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-remove-breakpoint (&key breakpoint-id)
  "Remove a breakpoint via Swank."
  (let ((form `(,(swank-sym "BREAK-REMOVE") ,breakpoint-id)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-list-breakpoints ()
  "List all breakpoints via Swank."
  (send-request (swank-sym "BREAK-LIST") :package "CL-USER" :thread t))

(defun swank-toggle-breakpoint (&key breakpoint-id)
  "Toggle breakpoint enabled state via Swank."
  ;; Swank doesn't have direct toggle; we'll remove and re-add
  ;; For now, just return success
  (declare (ignore breakpoint-id))
  (list :success t :message "Toggle not implemented"))

(defun swank-threads ()
  "List all threads in Swank-connected SBCL."
  (send-request (swank-sym "THREAD-LIST") :package "CL-USER" :thread t))

(defun swank-abort-thread (&key (thread-id :repl-thread))
  "Abort THREAD-ID (default: current REPL thread)."
  (let ((form `(,(swank-sym "ABORT-THREAD") ,thread-id)))
    (send-request form :package "CL-USER" :thread t)))

(defun swank-interrupt ()
  "Interrupt current evaluation."
  (send-request (swank-sym "INTERRUPT") :package "CL-USER" :thread t))

(defun swank-inspect-object (&key expression)
  "Inspect an object via Swank.
EXPRESSION should be a string that evaluates to an object."
  (unless expression
    (return-from swank-inspect-object (list :error t :message "expression is required")))
  (let ((obj (read-from-string expression)))
    (send-request `(,(swank-sym "INIT-INSPECTOR") ,obj) :package "CL-USER" :thread t)))

(defun swank-inspect-nth-part (&key (part-id 0))
  "Inspect the nth part of the current inspector."
  (send-request `(,(swank-sym "INSPECT-NTH-PART") ,part-id) :package "CL-USER" :thread t))

(defun swank-describe (&key symbol)
  "Describe an object via Swank."
  (unless symbol
    (return-from swank-describe (list :error t :message "symbol is required")))
  (let ((obj (read-from-string symbol)))
    (send-request `(,(swank-sym "DESCRIBE") ,obj) :package "CL-USER" :thread t)))

(defun swank-autodoc (&key symbol)
  "Get documentation for SYMBOL (string)."
  (unless symbol
    (return-from swank-autodoc (list :error t :message "symbol is required")))
  (send-request `(,(swank-sym "AUTODOC") ,symbol) :package "CL-USER" :thread t))

(defun swank-completions (&key prefix (package "CL-USER"))
  "Get symbol completions for PREFIX in PACKAGE."
  (unless prefix
    (return-from swank-completions (list :error t :message "prefix is required")))
  (send-request `(,(swank-sym "SIMPLE-COMPLETIONS") ,prefix ,package) :package "CL-USER" :thread t))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun get-unix-time ()
  "Get current Unix timestamp."
  #+sbcl
  (sb-ext:get-time-of-day)
  #+ccl
  (ccl:get-time-of-day)
  #-(or sbcl ccl)
  (- (/ (get-universal-time) 86400) 2))

;;; ============================================================
;;; Public API for MCP Tools
;;; ============================================================

(defun mcp-swank-eval (&key code (package "CL-USER"))
  "MCP tool handler: Evaluate Lisp code via Swank."
  (swank-eval :code code :package package))

(defun mcp-swank-compile (&key code (package "CL-USER") (filename "repl"))
  "MCP tool handler: Compile Lisp code via Swank."
  (swank-compile :code code :package package :filename filename))

(defun mcp-swank-threads ()
  "MCP tool handler: List all threads in Swank-connected SBCL."
  (swank-threads))

(defun mcp-swank-abort (&optional (thread-id :repl-thread))
  "MCP tool handler: Abort a thread."
  (swank-abort-thread thread-id))

(defun mcp-swank-interrupt ()
  "MCP tool handler: Interrupt current evaluation."
  (swank-interrupt))

(defun mcp-swank-backtrace (&optional (thread :repl-thread))
  "MCP tool handler: Get current backtrace."
  (declare (ignore thread))
  (swank-backtrace))

(defun mcp-swank-frame-locals (frame &optional (thread :repl-thread))
  "MCP tool handler: Get local variables for a frame."
  (swank-frame-locals frame thread))

(defun mcp-swank-inspect (expression)
  "MCP tool handler: Inspect an object via Swank."
  (swank-inspect-object expression))

(defun mcp-swank-describe (expression)
  "MCP tool handler: Describe an object via Swank."
  (swank-describe expression))

(defun mcp-swank-autodoc (symbol)
  "MCP tool handler: Get documentation for a symbol."
  (swank-autodoc symbol))

(defun mcp-swank-completions (symbol &optional (package "CL-USER"))
  "MCP tool handler: Get symbol completions via Swank."
  (swank-completions symbol package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Breakpoint MCP Tool Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mcp-swank-set-breakpoint (function &key condition hit-count thread)
  "MCP tool handler: Set a breakpoint via Swank."
  (swank-set-breakpoint :function function :condition condition :hit-count hit-count :thread thread))

(defun mcp-swank-remove-breakpoint (breakpoint-id)
  "MCP tool handler: Remove a breakpoint via Swank."
  (swank-remove-breakpoint :breakpoint-id breakpoint-id))

(defun mcp-swank-list-breakpoints ()
  "MCP tool handler: List all breakpoints via Swank."
  (swank-list-breakpoints))

(defun mcp-swank-toggle-breakpoint (breakpoint-id)
  "MCP tool handler: Toggle breakpoint enabled state via Swank."
  (swank-toggle-breakpoint :breakpoint-id breakpoint-id))

;;; Provide debugger event popping for the debugger tools
(defun pop-debugger-event ()
  "Pop the most recent debugger event from the queue.
Returns (values condition restarts frames) or NIL if none."
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (loop for i from (1- (length *event-queue*)) downto 0
          when (eq (swank-event-type (aref *event-queue* i)) :debug)
          do (let ((event (aref *event-queue* i))
                   (data (swank-event-data (aref *event-queue* i))))
               (deletef (aref *event-queue* i) *event-queue*)
               (return (values (getf data :condition)
                               (getf data :restarts)
                               (getf data :frames)))))))

;;; ============================================================
;;; Package Definition
;;; ============================================================

(provide :cl-tron-mcp/swank)
