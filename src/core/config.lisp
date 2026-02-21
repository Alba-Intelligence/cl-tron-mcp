;;;; src/core/config.lisp

(in-package :cl-tron-mcp/core)

(defparameter *config* (make-hash-table))

(defun get-config (key &optional default)
  (gethash key *config* default))

(defun set-config (key value)
  (setf (gethash key *config*) value))

(set-config :transport :stdio)
(set-config :port 8080)
(set-config :approval-timeout 300)
(set-config :debug nil)
