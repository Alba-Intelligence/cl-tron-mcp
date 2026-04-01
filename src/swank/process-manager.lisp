;;;; src/swank/process-manager.lisp
;;;;
;;;; Manages SBCL+Swank subprocesses spawned on behalf of an agent.
;;;; Allows an agent to launch a fresh SBCL image with Swank running on a
;;;; chosen port, inspect its status, and terminate it — without requiring
;;;; any manual setup by the user.
;;;;
;;;; Design:
;;;;   - UIOP:LAUNCH-PROGRAM spawns the child process
;;;;   - Port-ready polling (TCP connect every 0.5s up to timeout)
;;;;   - *managed-processes* hash: port (integer) → process-info plist
;;;;   - All state access serialised with *process-registry-lock*
;;;;   - :spawn communication style allows concurrent MCP + SLIME connections

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Process Registry
;;; ============================================================

(defvar *managed-processes* (make-hash-table)
  "Registry of SBCL processes launched by this MCP instance.
Key: port (integer). Value: plist with :process :pid :started-at :port :status.")

(defvar *process-registry-lock*
  (bordeaux-threads:make-lock "process-registry-lock")
  "Protects *managed-processes* from concurrent access.")

;;; ============================================================
;;; Port Availability Check
;;; ============================================================

(defun port-available-p (port &key (host "127.0.0.1"))
  "Return T if a TCP connection can be made to HOST:PORT (i.e. Swank is ready)."
  (handler-case
      (let ((sock (usocket:socket-connect host port :timeout 1)))
        (usocket:socket-close sock)
        t)
    (error () nil)))

(defun wait-for-port (port &key (host "127.0.0.1") (timeout 30) (interval 0.5))
  "Poll HOST:PORT every INTERVAL seconds until it accepts connections or TIMEOUT expires.
Returns T if the port became available, NIL on timeout."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (when (port-available-p port :host host)
        (return t))
      (when (>= (get-universal-time) deadline)
        (return nil))
      (sleep interval))))

;;; ============================================================
;;; Process Lifecycle
;;; ============================================================

(defun build-swank-init-forms (port communication-style extra-eval)
  "Build the --eval arguments for starting SBCL with Swank."
  (let ((forms
         (append
          (list "(ql:quickload :swank :silent t)"
                (format nil "(swank:create-server :port ~d :dont-close t :style :~(~a~))"
                        port communication-style))
          (when extra-eval
            (if (listp extra-eval) extra-eval (list extra-eval))))))
    (loop for form in forms
          nconc (list "--eval" form))))

(defun launch-sbcl-with-swank (&key (port 4006)
                                     (host "127.0.0.1")
                                     (communication-style :spawn)
                                     (timeout 30)
                                     (sbcl-binary "sbcl")
                                     extra-eval)
  "Launch a fresh SBCL image with Swank listening on PORT.

  COMMUNICATION-STYLE controls Swank concurrency:
    :spawn  — each connection gets its own thread (allows MCP + SLIME simultaneously)
    :fd-handler — single-threaded event loop
    :sigio  — signal-based (SBCL only)

  Returns a plist:
    :success t   :port <n>  :pid <n>  :message <str>   on success
    :error   t   :message   <str>                       on failure"
  (when (bordeaux-threads:with-lock-held (*process-registry-lock*)
          (gethash port *managed-processes*))
    (return-from launch-sbcl-with-swank
      (list :error t :code "PORT_ALREADY_MANAGED"
            :message (format nil "Port ~d is already managed by this MCP instance" port))))

  (when (port-available-p port :host host)
    (return-from launch-sbcl-with-swank
      (list :error t :code "PORT_ALREADY_IN_USE"
            :message (format nil "Port ~d is already in use (not by this MCP)" port))))

  (let* ((eval-args (build-swank-init-forms port communication-style extra-eval))
         (cmd (list* sbcl-binary "--non-interactive" "--noinform" eval-args))
         (process (handler-case
                      (uiop:launch-program cmd
                                           :output :stream
                                           :error-output :stream)
                    (error (e)
                      (return-from launch-sbcl-with-swank
                        (list :error t :code "LAUNCH_FAILED"
                              :message (format nil "Failed to launch ~a: ~a" sbcl-binary e)))))))

    ;; Register immediately so kill works even if port never comes up
    (bordeaux-threads:with-lock-held (*process-registry-lock*)
      (setf (gethash port *managed-processes*)
            (list :process process
                  :pid (uiop:process-info-pid process)
                  :port port
                  :host host
                  :started-at (get-universal-time)
                  :status :starting
                  :communication-style communication-style)))

    ;; Wait for Swank to accept connections
    (if (wait-for-port port :host host :timeout timeout)
        (progn
          (bordeaux-threads:with-lock-held (*process-registry-lock*)
            (let ((entry (gethash port *managed-processes*)))
              (when entry
                (setf (getf entry :status) :running)
                (setf (gethash port *managed-processes*) entry))))
          (let ((pid (uiop:process-info-pid process)))
            (list :success t
                  :port port
                  :pid pid
                  :message (format nil "SBCL+Swank running on port ~d (pid ~d, style :~(~a~))"
                                   port pid communication-style))))
        (progn
          ;; Timed out — kill the process and clean up
          (ignore-errors (uiop:terminate-process process :urgent t))
          (bordeaux-threads:with-lock-held (*process-registry-lock*)
            (remhash port *managed-processes*))
          (list :error t :code "SWANK_START_TIMEOUT"
                :message (format nil "SBCL started but Swank did not accept connections on port ~d within ~d seconds"
                                 port timeout))))))

(defun kill-managed-process (&key port)
  "Terminate the managed SBCL process listening on PORT.
Returns a plist :success t / :error t."
  (let ((entry (bordeaux-threads:with-lock-held (*process-registry-lock*)
                 (gethash port *managed-processes*))))
    (unless entry
      (return-from kill-managed-process
        (list :error t :code "PROCESS_NOT_FOUND"
              :message (format nil "No managed process on port ~d" port))))

    (let ((process (getf entry :process))
          (pid     (getf entry :pid)))
      (handler-case
          (progn
            (uiop:terminate-process process :urgent nil)
            ;; Give it 3 seconds to exit gracefully, then force-kill
            (loop repeat 6
                  until (not (uiop:process-alive-p process))
                  do (sleep 0.5))
            (when (uiop:process-alive-p process)
              (uiop:terminate-process process :urgent t)))
        (error (e)
          (cl-tron-mcp/logging:log-warn
           (format nil "Error terminating process on port ~d: ~a" port e))))

      (bordeaux-threads:with-lock-held (*process-registry-lock*)
        (remhash port *managed-processes*))

      (list :success t
            :port port
            :pid pid
            :message (format nil "Terminated SBCL process (pid ~d) on port ~d" pid port)))))

;;; ============================================================
;;; Status & Listing
;;; ============================================================

(defun process-uptime (started-at)
  "Return uptime in seconds."
  (- (get-universal-time) started-at))

(defun process-info-plist (entry)
  "Convert internal process entry to a user-facing plist."
  (let* ((process (getf entry :process))
         (alive (ignore-errors (uiop:process-alive-p process)))
         (started (getf entry :started-at))
         (status (if alive
                     (symbol-name (getf entry :status))
                     "dead")))
    (list :port    (getf entry :port)
          :pid     (getf entry :pid)
          :host    (getf entry :host)
          :status  status
          :style   (symbol-name (getf entry :communication-style))
          :uptime  (when started (process-uptime started))
          :started-at started)))

(defun managed-process-status (&key port)
  "Return status plist for the process on PORT, or an error plist."
  (let ((entry (bordeaux-threads:with-lock-held (*process-registry-lock*)
                 (gethash port *managed-processes*))))
    (if entry
        (list :success t :process (process-info-plist entry))
        (list :error t :code "PROCESS_NOT_FOUND"
              :message (format nil "No managed process on port ~d" port)))))

(defun list-managed-processes ()
  "Return a list of status plists for all managed processes."
  (let (result)
    (bordeaux-threads:with-lock-held (*process-registry-lock*)
      (maphash (lambda (port entry)
                 (declare (ignore port))
                 (push (process-info-plist entry) result))
               *managed-processes*))
    (list :success t
          :count (length result)
          :processes (sort result #'< :key (lambda (p) (or (getf p :port) 0))))))
