;;;; src/transport/http.lisp

(in-package :cl-tron-mcp/transport)

#+hunchentoot
(progn
  (defvar *acceptor* nil)

  (defun start-transport (&key (port 8080) (handler #'cl-tron-mcp/protocol:handle-message))
    "Start HTTP transport."
    (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                     :port port
                                     :document-root #P"/tmp/"))
    (hunchentoot:start *acceptor*))

  (defun stop-transport ()
    "Stop HTTP transport."
    (when *acceptor*
      (hunchentoot:stop *acceptor*))))
