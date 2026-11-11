;;;; tests/read-eval-test.lisp - agent-supplied text must not run code at read time (S6)

(in-package :cl-tron-mcp/tests)

(defvar cl-user::*tron-read-eval-probe* nil)

(defmacro probe-does-not-fire (&body calls)
  "Run CALLS with a #. payload and assert the read-time side effect never happened."
  `(progn
     (setf cl-user::*tron-read-eval-probe* nil)
     ,@(mapcar (lambda (c) `(ignore-errors ,c)) calls)
     (ok (null cl-user::*tron-read-eval-probe*))))

(defparameter *probe-payload* "#.(setq cl-user::*tron-read-eval-probe* t)")

(deftest no-read-time-eval-in-backends-test
  (testing "tracer"
    (probe-does-not-fire (cl-tron-mcp/tracer:trace-function *probe-payload*)))
  (testing "xref"
    (probe-does-not-fire (cl-tron-mcp/xref:who-calls *probe-payload*)))
  (testing "compile"
    (probe-does-not-fire (cl-tron-mcp/sbcl:safe-compile *probe-payload*)))
  (testing "repl compile-and-load"
    (probe-does-not-fire (cl-tron-mcp/repl:repl-compile-and-load *probe-payload*)))
  (testing "hot-reload"
    (probe-does-not-fire (cl-tron-mcp/hot-reload:replace-function *probe-payload* "(lambda () 1)"))))
