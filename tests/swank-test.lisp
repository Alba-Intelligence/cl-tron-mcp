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
              (let ((result (cl-tron-mcp/tools:call-tool "swank_debugger_state" nil)))
                (ok (listp result))
                (ok (= 123 (getf result :thread)))
                (ok (= 2 (getf result :level)))
                (ok (getf result :in-debugger))))))

(deftest repl-get-restarts-tool-test
    (testing "repl_get_restarts accepts frame argument without signaling an error"
             (let ((cl-tron-mcp/unified::*repl-connected* t)
                  (cl-tron-mcp/swank::*debugger-thread* 456)
                  (cl-tron-mcp/swank::*debugger-level* 1))
              (bordeaux-threads:with-lock-held (cl-tron-mcp/swank::*event-mutex*)
                (setf (fill-pointer cl-tron-mcp/swank::*event-queue*) 0))
              (cl-tron-mcp/swank::enqueue-debugger-event
               "test condition"
               '(("RETRY" "Retry request") ("ABORT" "Abort request"))
               '((0 "(CAR 42)")))
              (let ((result (handler-case
                                (cl-tron-mcp/tools:call-tool "repl_get_restarts" (list :|frame| 0))
                              (error (e) e))))
                (ok (listp result))
                (ok (equal '(("RETRY" "Retry request") ("ABORT" "Abort request"))
                           (getf result :restarts)))
                (ok (= 456 (getf result :thread)))
                (ok (= 1 (getf result :level)))))))

(deftest tool-camelcase-argument-normalization-test
    (testing "camelCase MCP argument names are normalized to the snake_case keywords handlers validate"
             (let ((original (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart)))
               (unwind-protect
                   (progn
                     (setf (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart)
                           (lambda (&key restart_index)
                             (list :restart_index restart_index)))
                     (let ((result (handler-case
                                       (cl-tron-mcp/tools:call-tool "repl_invoke_restart" (list :|restartIndex| 2))
                                     (error (e) e))))
                       (ok (listp result))
                       (ok (= 2 (getf result :restart_index)))))
                (setf (symbol-function 'cl-tron-mcp/unified:repl-invoke-restart) original)))))

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
