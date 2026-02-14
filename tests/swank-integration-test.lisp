;;;; tests/swank-integration-test.lisp - Swank integration tests
;;;;
;;;; These tests require a running Swank server on port 4005.
;;;; Run with: (rove:run 'cl-tron-mcp/tests/swank-integration)
;;;;
;;;; To start a Swank server:
;;;;   sbcl --eval '(ql:quickload :swank)' --eval '(swank:create-server :port 4005 :dont-close t)'

(defpackage :cl-tron-mcp/tests/swank-integration
  (:use #:cl #:rove))

(in-package :cl-tron-mcp/tests/swank-integration)

(defvar *swank-port* 4005
  "Port for Swank server (default 4005).")

(defvar *swank-available* nil
  "Cached result of swank availability check.")

(defun swank-available-p ()
  "Check if Swank server is available on *swank-port*."
  (or *swank-available*
      (setf *swank-available*
            (handler-case
                (let ((socket (usocket:socket-connect "127.0.0.1" *swank-port* :timeout 2)))
                  (usocket:socket-close socket)
                  t)
              (error () nil)))))

(defun ensure-disconnected ()
  "Ensure we're disconnected from Swank before/after tests."
  (when (cl-tron-mcp/swank:swank-connected-p)
    (cl-tron-mcp/swank:swank-disconnect)))

(defmacro with-swank-connection (&body body)
  "Execute BODY with Swank connection, ensuring cleanup."
  `(if (not (swank-available-p))
       (testing "Swank server not available - skipping" (ok t))
       (progn
         (ensure-disconnected)
         (cl-tron-mcp/swank:swank-connect :port *swank-port*)
         (unwind-protect (progn ,@body)
           (cl-tron-mcp/swank:swank-disconnect)))))

(deftest swank-connection-test
  (testing "Connect to Swank server"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-connect :port *swank-port*)))
        (ok (getf result :error) "Should report already connected")))))

(deftest swank-eval-test
  (testing "Evaluate simple expression"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-eval :code "(+ 1 2 3)")))
        (ok (getf result :result))
        (ok (listp (getf result :result)))))))

(deftest swank-backtrace-test
  (testing "Get backtrace"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-backtrace :start 0 :end 5)))
        (ok (getf result :result))))))

(deftest swank-threads-test
  (testing "List threads"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-threads)))
        (ok (getf result :result))
        (let ((threads (second (getf result :result))))
          (ok (listp threads))
          (ok (> (length threads) 0)))))))

(deftest swank-autodoc-test
  (testing "Get arglist for symbol"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-autodoc :symbol "mapcar")))
        (ok (getf result :result))))))

(deftest swank-completions-test
  (testing "Get symbol completions"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-completions :prefix "map" :package "CL")))
        (ok (getf result :result))
        (let ((completions (second (getf result :result))))
          (ok (listp completions))
          (ok (> (length completions) 0)))))))

(deftest swank-debugger-test
  (testing "Trigger error and get debug state"
    (with-swank-connection
      (let ((result (cl-tron-mcp/swank:swank-eval :code "(car 42)")))
        (ok (getf result :result))
        (let ((res (getf result :result)))
          (ok (getf res :debug))))
      (multiple-value-bind (thread level in-debugger)
          (cl-tron-mcp/swank:swank-debugger-state)
        (ok thread "Debugger thread should be set")
        (ok (> level 0) "Debugger level should be > 0")
        (ok in-debugger "Should be in debugger"))
      (let ((restarts (cl-tron-mcp/swank:swank-get-restarts)))
        (ok (getf restarts :restarts))
        (ok (> (length (getf restarts :restarts)) 0)))
      (cl-tron-mcp/swank:swank-invoke-restart :restart_index 1))))
