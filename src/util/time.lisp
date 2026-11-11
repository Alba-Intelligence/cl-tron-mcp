;;;; src/util/time.lisp - small shared helpers (one definition per helper, review finding D1)

(defpackage :cl-tron-mcp/util
  (:use :cl)
  (:export #:get-unix-time))

(in-package :cl-tron-mcp/util)

(defconstant +unix-epoch-universal-time+ 2208988800
  "Universal time (seconds since 1900-01-01) of the Unix epoch.")

(defun get-unix-time ()
  "Current time as whole seconds since the Unix epoch (portable; no dependency on local-time)."
  (- (get-universal-time) +unix-epoch-universal-time+))
