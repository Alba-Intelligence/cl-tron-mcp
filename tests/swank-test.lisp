;;;; tests/swank-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest swank-connection-state-test
    (testing "Swank connection state variables exist"
             (ok (boundp 'cl-tron-mcp/swank::*swank-connected*))
             (ok (boundp 'cl-tron-mcp/swank::*swank-socket*))
             (ok (boundp 'cl-tron-mcp/swank::*swank-io*))))

(deftest swank-error-handling-test
    (testing "Swank functions return errors when not connected"
             ;; These should return error responses
             (let ((result (cl-tron-mcp/swank:mcp-swank-threads)))
               (ok (getf result :error)))
             (let ((result (cl-tron-mcp/swank:mcp-swank-backtrace)))
               (ok (getf result :error)))
             (let ((result (cl-tron-mcp/swank:mcp-swank-completions :prefix "mak")))
               (ok (getf result :error)))))

(deftest swank-debugger-state-tool-test
    (testing "swank_debugger_state returns structured debugger state"
             (let ((cl-tron-mcp/swank::*debugger-thread* 123)
                  (cl-tron-mcp/swank::*debugger-level* 2))
              (let ((result (cl-tron-mcp/tools::call-tool "swank_debugger_state" nil)))
                (ok (listp result))
                (ok (= 123 (getf result :thread)))
                (ok (= 2 (getf result :level)))
                (ok (getf result :in-debugger))))))

(deftest repl-get-restarts-tool-test
    (testing "repl_get_restarts accepts frame argument without signaling an error"
             (let ((cl-tron-mcp/tools::*repl-connected* t)
                  (cl-tron-mcp/swank::*debugger-thread* 456)
                  (cl-tron-mcp/swank::*debugger-level* 1))
              (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
                (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
              (cl-tron-mcp/swank::enqueue-debugger-event
               "test condition"
               '(("RETRY" "Retry request") ("ABORT" "Abort request"))
               '((0 "(CAR 42)")))
              (let ((result (handler-case
                                (cl-tron-mcp/tools::call-tool "repl_get_restarts" (list :|frame| 0))
                              (error (e) e))))
                (ok (listp result))
                (ok (equal '(("RETRY" "Retry request") ("ABORT" "Abort request"))
                           (getf result :restarts)))
                (ok (= 456 (getf result :thread)))
                (ok (= 1 (getf result :level)))))))

(deftest tool-camelcase-argument-normalization-test
    (testing "camelCase MCP argument names are normalized to the snake_case keywords handlers validate"
             (let ((original (symbol-function 'cl-tron-mcp/tools::repl-invoke-restart)))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'cl-tron-mcp/tools::repl-invoke-restart)
                           (lambda (&key restart_index)
                             (list :restart_index restart_index)))
                     (let ((result (handler-case
                                       (cl-tron-mcp/tools::call-tool "repl_invoke_restart" (list :|restartIndex| 2))
                                     (error (e) e))))
                       (ok (listp result))
                       (ok (= 2 (getf result :restart_index)))))
                (setf (symbol-function 'cl-tron-mcp/tools::repl-invoke-restart) original)))))

(deftest swank-invoke-restart-clears-debugger-state-test
    (testing "swank-invoke-restart clears cached debugger state when Swank reports abort"
             (let ((original (symbol-function 'cl-tron-mcp/swank::send-request))
                  (cl-tron-mcp/swank::*debugger-thread* 789)
                  (cl-tron-mcp/swank::*debugger-level* 1))
               (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
                (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
               (cl-tron-mcp/swank::enqueue-debugger-event
                "test condition"
                '(("ABORT" "Abort request"))
                '((0 "(CAR 42)")))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'cl-tron-mcp/swank::send-request)
                           (lambda (&rest args)
                             (declare (ignore args))
                             (list :result (list :abort nil))))
                     (let ((result (cl-tron-mcp/swank:swank-invoke-restart :restart_index 1)))
                       (ok (listp result))
                       (ok (null cl-tron-mcp/swank::*debugger-thread*))
                       (ok (zerop cl-tron-mcp/swank::*debugger-level*))
                       (ok (getf (cl-tron-mcp/swank:swank-get-restarts) :error))))
                (setf (symbol-function 'cl-tron-mcp/swank::send-request) original)))))

;;; Event queue overflow (P6): never deadlock, never lose debugger events, report what was dropped.

(defmacro with-clean-event-queue (&body body)
  `(let ((cl-tron-mcp/swank::*max-event-queue-size* 10))
     (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
       (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
     (setf cl-tron-mcp/swank::*dropped-event-count* 0)
     (unwind-protect (progn ,@body)
       (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
         (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
       (setf cl-tron-mcp/swank::*dropped-event-count* 0))))

(deftest event-queue-overflow-keeps-debug-events-test
  (with-clean-event-queue
    (testing "overflowing with output events does not deadlock and keeps the queue bounded"
      (cl-tron-mcp/swank::enqueue-debugger-event "boom" '(("ABORT" "abort")) '())
      (dotimes (i 25)
        (cl-tron-mcp/swank::enqueue-output-event (format nil "line ~d" i) :repl))
      (ok (<= (length cl-tron-mcp/swank::*event-queue*) 10)))
    (testing "the debugger event survived and the oldest output was what got dropped"
      (ok (find :debug cl-tron-mcp/swank::*event-queue*
                :key #'cl-tron-mcp/swank::swank-event-type))
      (ok (= 16 cl-tron-mcp/swank::*dropped-event-count*))
      (let ((outputs (loop for e across cl-tron-mcp/swank::*event-queue*
                           when (eq :output (cl-tron-mcp/swank::swank-event-type e))
                             collect (getf (cl-tron-mcp/swank::swank-event-data e) :string))))
        (ok (equal "line 24" (car (last outputs))))
        (ok (not (member "line 0" outputs :test #'equal)))))))

(deftest event-queue-all-debug-still-bounded-test
  (with-clean-event-queue
    (testing "even a queue full of debugger events stays bounded (oldest is dropped last-resort)"
      (dotimes (i 15)
        (cl-tron-mcp/swank::enqueue-debugger-event (format nil "c~d" i) '() '()))
      (ok (<= (length cl-tron-mcp/swank::*event-queue*) 10))
      (ok (= 5 cl-tron-mcp/swank::*dropped-event-count*)))))

(deftest debugger-state-tool-reports-dropped-events-test
  (with-clean-event-queue
    (testing "swank_debugger_state tells the agent how many events were dropped"
      (setf cl-tron-mcp/swank::*dropped-event-count* 7)
      (let ((result (cl-tron-mcp/tools::call-tool "swank_debugger_state" nil)))
        (ok (= 7 (getf result :dropped-events)))))))

;;; Managed process output (P7): a chatty child must not fill an undrained pipe and stall.

(deftest managed-process-output-goes-to-a-log-file-test
  (testing "A child that prints far more than a pipe buffer still starts, and its output is in the log"
    (let ((result (cl-tron-mcp/swank:launch-sbcl-with-swank
                   :port 4057 :timeout 60
                   :extra-eval "(dotimes (i 20000) (format t \"chatty line ~d~%\" i))")))
      (unwind-protect
           (progn
             (ok (getf result :success))
             (ok (stringp (getf result :log-file)))
             (when (getf result :log-file)
               (sleep 1)
               (ok (probe-file (getf result :log-file)))
               (ok (search "chatty line 19999"
                           (uiop:read-file-string (getf result :log-file))))))
        (cl-tron-mcp/swank:kill-managed-process :port 4057)))))

;;; B5: an unanswered request must time out instead of blocking forever.

(deftest wait-for-response-times-out-test
  (testing "A request that is never answered returns a timeout error at the deadline"
    (let* ((id 987654)
           (req (cl-tron-mcp/swank::make-swank-request
                 :id id :condition (bordeaux-threads:make-condition-variable)
                 :response nil :completed-p nil))
           (start (get-internal-real-time)))
      (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*request-lock*)
        (setf (gethash id cl-tron-mcp/swank::*pending-requests*) req))
      (let ((result (cl-tron-mcp/swank::wait-for-response id :timeout 1))
            (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
        (ok (getf result :timeout))
        (ok (getf result :error))
        (ok (< elapsed 4) "gave up promptly")
        (ok (null (gethash id cl-tron-mcp/swank::*pending-requests*))
            "the abandoned request is no longer tracked")))))
