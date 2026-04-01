;;;; src/swank/swank-api.lisp - High-level Swank RPC operations and MCP wrappers
;;;;
;;;; This file implements the Swank RPC operations used by MCP tools plus the
;;;; thin mcp-swank-* wrapper functions registered in swank-tools.lisp.
;;;;
;;;; Load order: loaded after swank-connection.lisp, swank-rpc.lisp, swank-events.lisp

(in-package #:cl-tron-mcp/swank)

;;; ============================================================
;;; Code Evaluation
;;; ============================================================

(defun swank-eval (&key code (package "CL-USER"))
  "Evaluate Lisp code string via Swank (swank:eval-and-grab-output)."
  (unless (and code (stringp code) (plusp (length code)))
    (return-from swank-eval
      (cl-tron-mcp/core:make-error-with-hint "INVALID_CODE_PARAMETER"
                                             :details (list :function "swank-eval"))))
  (unless (and package (stringp package) (plusp (length package)))
    (return-from swank-eval
      (cl-tron-mcp/core:make-error-with-hint "INVALID_PACKAGE_PARAMETER"
                                             :details (list :function "swank-eval"))))
  (send-request `(,(swank-sym "EVAL-AND-GRAB-OUTPUT") ,code)
                :package package :thread t))

(defun swank-compile (&key code (package "CL-USER") (filename "repl"))
  "Compile Lisp code string via Swank (swank:compile-string-for-emacs)."
  (unless (and code (stringp code) (plusp (length code)))
    (return-from swank-compile
      (cl-tron-mcp/core:make-error-with-hint "INVALID_CODE_PARAMETER"
                                             :details (list :function "swank-compile"))))
  (unless (and package (stringp package) (plusp (length package)))
    (return-from swank-compile
      (cl-tron-mcp/core:make-error-with-hint "INVALID_PACKAGE_PARAMETER"
                                             :details (list :function "swank-compile"))))
  (unless (and filename (stringp filename) (plusp (length filename)))
    (return-from swank-compile
      (cl-tron-mcp/core:make-error-with-hint "INVALID_FILENAME_PARAMETER"
                                             :details (list :function "swank-compile"))))
  (send-request `(,(swank-sym "COMPILE-STRING-FOR-EMACS") ,code ,filename nil ,filename nil)
                :package package :thread t))

;;; ============================================================
;;; Backtraces and Frames
;;; ============================================================

(defun swank-backtrace (&key (start 0) (end 20))
  "Get backtrace from Swank using sb-debug:list-backtrace."
  (send-request
   `(,(swank-sym "EVAL-AND-GRAB-OUTPUT")
      (format nil "(sb-debug:list-backtrace :start ~a :count ~a)" ,start ,(- end start)))
   :package "CL-USER" :thread t))

(defun swank-frame-locals (&key (frame-index 0) (thread :repl-thread))
  "Get local variables for FRAME-INDEX."
  (send-request `(,(swank-sym "FRAME-LOCALS-AND-CATCH-TAGS") ,frame-index)
                :package "CL-USER" :thread thread))

(defun swank-eval-in-frame (&key code (frame-index 0) (package "CL-USER"))
  "Evaluate CODE (string) in the context of FRAME-INDEX."
  (unless (and code (stringp code) (plusp (length code)))
    (return-from swank-eval-in-frame
      (cl-tron-mcp/core:make-error-with-hint "INVALID_CODE_PARAMETER"
                                             :details (list :function "swank-eval-in-frame"))))
  (unless (and package (stringp package) (plusp (length package)))
    (return-from swank-eval-in-frame
      (cl-tron-mcp/core:make-error-with-hint "INVALID_PACKAGE_PARAMETER"
                                             :details (list :function "swank-eval-in-frame"))))
  (unless (and (integerp frame-index) (>= frame-index 0))
    (return-from swank-eval-in-frame
      (cl-tron-mcp/core:make-error-with-hint "INVALID_FRAME_INDEX"
                                             :details (list :function "swank-eval-in-frame"))))
  (send-request `(,(swank-sym "EVAL-STRING-IN-FRAME") ,code ,frame-index ,package)
                :package package :thread t))

;;; ============================================================
;;; Debugger Operations
;;; ============================================================

(defun swank-invoke-restart (&key (restart_index 1))
  "Invoke the Nth restart (1-based) in the current debugger level."
  (let ((thread (or *debugger-thread* t))
        (level *debugger-level*))
    (send-request `(,(swank-sym "INVOKE-NTH-RESTART-FOR-EMACS") ,level ,restart_index)
                  :package "CL-USER" :thread thread)))

(defun swank-get-restarts (&optional (frame-index 0))
  "Return available restarts from the most recent cached :debug event."
  (declare (ignore frame-index))
  (bordeaux-threads:with-lock-held (*event-mutex*)
    (let ((debug-event (find :debug *event-queue*
                             :key #'swank-event-type :from-end t)))
      (if debug-event
          (list :restarts (getf (swank-event-data debug-event) :restarts)
                :thread *debugger-thread*
                :level *debugger-level*)
          (cl-tron-mcp/core:make-error "NO_DEBUGGER_EVENT")))))

(defun swank-debugger-state ()
  "Return (values thread-id level in-debugger-p)."
  (values *debugger-thread* *debugger-level* (not (null *debugger-thread*))))

(defun swank-continue ()
  "Continue execution from debugger."
  (send-request `(,(swank-sym "SLDB-CONTINUE"))
                :package "CL-USER"
                :thread (or *debugger-thread* t)))

;;; ============================================================
;;; Stepping
;;; ============================================================

(defun %invoke-step-restart (restart-name)
  "Internal: invoke a named step restart."
  (let ((thread (or *debugger-thread* t))
        (level *debugger-level*))
    (let ((restarts (swank-get-restarts)))
      (let ((pos (position restart-name (getf restarts :restarts)
                           :key #'first :test #'string=)))
        (if pos
            (send-request `(,(swank-sym "INVOKE-NTH-RESTART-FOR-EMACS") ,level ,(1+ pos))
                          :package "CL-USER" :thread thread)
            (cl-tron-mcp/core:make-error "NOT_IN_STEPPER"))))))

(defun swank-step (&key (frame-index 0))
  "Step into next expression."
  (declare (ignore frame-index))
  (%invoke-step-restart "STEP-INTO"))

(defun swank-next (&key (frame-index 0))
  "Step over next expression."
  (declare (ignore frame-index))
  (%invoke-step-restart "STEP-NEXT"))

(defun swank-out (&key (frame-index 0))
  "Step out of current frame."
  (declare (ignore frame-index))
  (%invoke-step-restart "STEP-OUT"))

(defun swank-step-continue ()
  "Exit stepping mode and continue normal execution."
  (let ((restarts (swank-get-restarts)))
    (let ((pos (position "STEP-CONTINUE" (getf restarts :restarts)
                         :key #'first :test #'string=)))
      (if pos
          (let ((thread (or *debugger-thread* t))
                (level *debugger-level*))
            (send-request `(,(swank-sym "INVOKE-NTH-RESTART-FOR-EMACS") ,level ,(1+ pos))
                          :package "CL-USER" :thread thread))
          (swank-continue)))))

;;; ============================================================
;;; Breakpoints
;;; ============================================================

(defun swank-set-breakpoint (&key function condition hit-count thread)
  "Set a breakpoint on FUNCTION via Swank.
When CONDITION (a Lisp expression string) or HIT-COUNT is given, wraps
the function with a generated predicate evaluated in the remote image
instead of using swank:break directly."
  (declare (ignore thread))
  (if (or condition hit-count)
      ;; Conditional/counted breakpoint: install via eval in remote image
      (let* ((hit-count-var (when hit-count (gensym "HIT-COUNT-")))
             (wrapper-code
               (format nil
                 "(let (~a)
                    (sb-int:encapsulate '~a 'mcp-breakpoint
                      (lambda (fn &rest args)
                        ~a
                        (apply fn args))))"
                 (if hit-count
                     (format nil "(~a 0)" hit-count-var)
                     "")
                 function
                 (cond
                   ((and condition hit-count)
                    (format nil
                      "(when (and (progn ~a) (>= (incf ~a) ~d)) (break \"MCP breakpoint on ~a (hit ~d)\"))"
                      condition hit-count-var hit-count function hit-count))
                   (condition
                    (format nil "(when (progn ~a) (break \"MCP breakpoint on ~a\"))"
                            condition function))
                   (hit-count
                    (format nil "(when (>= (incf ~a) ~d) (break \"MCP breakpoint on ~a (hit ~d)\"))"
                            hit-count-var hit-count function hit-count))))))
        (send-request `(,(swank-sym "EVAL-AND-GRAB-OUTPUT") ,wrapper-code)
                      :package "CL-USER" :thread t))
      ;; Simple unconditional breakpoint via swank:break
      (send-request `(,(swank-sym "BREAK") ,function)
                    :package "CL-USER" :thread t)))

(defun swank-remove-breakpoint (&key breakpoint-id)
  "Remove a breakpoint via Swank."
  (send-request `(,(swank-sym "BREAK-REMOVE") ,breakpoint-id)
                :package "CL-USER" :thread t))

(defun swank-unencapsulate (&key function)
  "Remove a conditional MCP breakpoint encapsulation from FUNCTION."
  (send-request `(,(swank-sym "EVAL-AND-GRAB-OUTPUT")
                  ,(format nil "(sb-int:unencapsulate '~a 'mcp-breakpoint)" function))
                :package "CL-USER" :thread t))

(defun swank-list-breakpoints ()
  "List all breakpoints via Swank."
  (send-request `(,(swank-sym "BREAK-LIST")) :package "CL-USER" :thread t))

(defun swank-toggle-breakpoint (&key breakpoint-id)
  "Toggle breakpoint state — callers should use breakpoints.lisp:toggle-breakpoint
which implements this via remove/re-add using the saved breakpoint state."
  (declare (ignore breakpoint-id))
  (list :error t :message "Use cl-tron-mcp/debugger:toggle-breakpoint instead"))

;;; ============================================================
;;; Thread Management
;;; ============================================================

(defun swank-threads ()
  "List all threads in the Swank-connected SBCL."
  (send-request `(,(swank-sym "LIST-THREADS")) :package "CL-USER" :thread t))

(defun swank-abort-thread (&key (thread-id :repl-thread))
  "Abort THREAD-ID (default: current REPL thread)."
  (send-request `(,(swank-sym "ABORT-THREAD") ,thread-id)
                :package "CL-USER" :thread t))

(defun swank-interrupt ()
  "Interrupt the current evaluation."
  (handler-case
      (progn
        (log-info "Sending interrupt to Swank")
        (let ((result (send-request `(,(swank-sym "INTERRUPT")) :package "CL-USER" :thread t)))
          (if (getf result :error)
              (progn (log-error (format nil "Interrupt failed: ~a" (getf result :message))) result)
              (progn (log-info "Interrupt sent successfully") result))))
    (error (e)
      (log-error (format nil "Interrupt error: ~a" e))
      (cl-tron-mcp/core:make-error "INTERRUPT_ERROR"
                                   :details (list :error (princ-to-string e))))))

;;; ============================================================
;;; Inspection and Documentation
;;; ============================================================

(defun swank-inspect-object (&key expression)
  "Inspect an object via Swank. EXPRESSION is a string."
  (unless (and expression (stringp expression) (plusp (length expression)))
    (return-from swank-inspect-object
      (cl-tron-mcp/core:make-error-with-hint "INVALID_EXPRESSION_PARAMETER"
                                             :details (list :function "swank-inspect-object"))))
  (send-request `(,(swank-sym "EVAL-AND-GRAB-OUTPUT")
                   (,(swank-sym "INSPECT-IN-EMACS") ,expression))
                :package "CL-USER" :thread t))

(defun swank-inspect-nth-part (&key (part-id 0))
  "Inspect the nth part of the current inspector view."
  (send-request `(,(swank-sym "INSPECT-NTH-PART") ,part-id) :package "CL-USER" :thread t))

(defun swank-describe (&key symbol)
  "Describe a symbol via Swank."
  (unless (and symbol (stringp symbol) (plusp (length symbol)))
    (return-from swank-describe
      (cl-tron-mcp/core:make-error-with-hint "INVALID_SYMBOL_PARAMETER"
                                             :details (list :function "swank-describe"))))
  (send-request `(,(swank-sym "DESCRIBE-SYMBOL") ,symbol) :package "CL-USER" :thread t))

(defun swank-autodoc (&key symbol)
  "Get arglist/documentation for SYMBOL string."
  (unless (and symbol (stringp symbol) (plusp (length symbol)))
    (return-from swank-autodoc
      (cl-tron-mcp/core:make-error-with-hint "INVALID_SYMBOL_PARAMETER"
                                             :details (list :function "swank-autodoc"))))
  (send-request `(,(swank-sym "EVAL-AND-GRAB-OUTPUT")
                   (format nil "(swank/backend:arglist (quote ~a))" ,symbol))
                :package "CL-USER" :thread t))

(defun swank-completions (&key prefix (package "CL-USER"))
  "Get symbol completions for PREFIX in PACKAGE."
  (unless (and prefix (stringp prefix) (plusp (length prefix)))
    (return-from swank-completions
      (cl-tron-mcp/core:make-error-with-hint "INVALID_PREFIX_PARAMETER"
                                             :details (list :function "swank-completions"))))
  (unless (and package (stringp package) (plusp (length package)))
    (return-from swank-completions
      (cl-tron-mcp/core:make-error-with-hint "INVALID_PACKAGE_PARAMETER"
                                             :details (list :function "swank-completions"))))
  (send-request `(,(swank-sym "SIMPLE-COMPLETIONS") ,prefix ,package)
                :package "CL-USER" :thread t))

;;; ============================================================
;;; Input Handling
;;; ============================================================

(defun swank-provide-input (&key input)
  "Respond to a pending Swank :read-string request with INPUT string.
Call this after the MCP server has received input from the user.

Returns: success plist or error plist."
  (unless (stringp input)
    (return-from swank-provide-input
      (cl-tron-mcp/core:make-error "INVALID_INPUT_PARAMETER")))
  (let ((pending (bordeaux-threads:with-lock-held (*input-request-lock*)
                   (pop *pending-input-requests*))))
    (unless pending
      (return-from swank-provide-input
        (cl-tron-mcp/core:make-error "NO_PENDING_INPUT_REQUEST")))
    (let ((thread-id (car pending))
          (tag       (cdr pending)))
      (handler-case
          (progn
            (write-message `(:emacs-return-string ,thread-id ,tag ,input))
            (log-info (format nil "Input sent to Swank (thread ~a tag ~a)" thread-id tag))
            (list :success t
                  :message (format nil "Input '~a' sent to Swank" input)))
        (error (e)
          (cl-tron-mcp/core:make-error "SWANK_WRITE_ERROR"
                                       :details (list :error (princ-to-string e))))))))

;;; ============================================================
;;; MCP Tool Wrapper Functions
;;; ============================================================

(defun mcp-swank-eval (&key code (package "CL-USER"))
  "MCP tool handler: evaluate Lisp code via Swank."
  (swank-eval :code code :package package))

(defun mcp-swank-compile (&key code (package "CL-USER") (filename "repl"))
  "MCP tool handler: compile Lisp code via Swank."
  (swank-compile :code code :package package :filename filename))

(defun mcp-swank-threads ()
  "MCP tool handler: list all threads in Swank-connected SBCL."
  (swank-threads))

(defun mcp-swank-abort (&key (thread-id :repl-thread))
  "MCP tool handler: abort a thread."
  (swank-abort-thread :thread-id thread-id))

(defun mcp-swank-interrupt ()
  "MCP tool handler: interrupt current evaluation."
  (swank-interrupt))

(defun mcp-swank-backtrace (&key (start 0) (end 20))
  "MCP tool handler: get current backtrace."
  (swank-backtrace :start start :end end))

(defun mcp-swank-frame-locals (&key frame (thread :repl-thread))
  "MCP tool handler: get local variables for a frame."
  (swank-frame-locals :frame-index frame :thread thread))

(defun mcp-swank-inspect (&key expression)
  "MCP tool handler: inspect an object via Swank."
  (swank-inspect-object :expression expression))

(defun mcp-swank-describe (&key expression)
  "MCP tool handler: describe an object via Swank."
  (swank-describe :symbol expression))

(defun mcp-swank-autodoc (&key symbol)
  "MCP tool handler: get documentation for a symbol."
  (swank-autodoc :symbol symbol))

(defun mcp-swank-completions (&key prefix (package "CL-USER"))
  "MCP tool handler: get symbol completions via Swank."
  (swank-completions :prefix prefix :package package))

(defun mcp-swank-set-breakpoint (&key function condition hit-count thread)
  "MCP tool handler: set a breakpoint via Swank."
  (swank-set-breakpoint :function function :condition condition
                        :hit-count hit-count :thread thread))

(defun mcp-swank-remove-breakpoint (&key breakpoint-id)
  "MCP tool handler: remove a breakpoint via Swank."
  (swank-remove-breakpoint :breakpoint-id breakpoint-id))

(defun mcp-swank-list-breakpoints ()
  "MCP tool handler: list all breakpoints via Swank."
  (swank-list-breakpoints))

(defun mcp-swank-toggle-breakpoint (&key breakpoint-id)
  "MCP tool handler: toggle breakpoint enabled state via Swank."
  (swank-toggle-breakpoint :breakpoint-id breakpoint-id))

(defun mcp-swank-send-input (&key input)
  "MCP tool handler: provide input string to a pending Swank :read-string request."
  (swank-provide-input :input input))
