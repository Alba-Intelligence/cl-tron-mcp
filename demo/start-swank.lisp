;;;; start-swank.lisp - Start Swank server for demo

(ql:quickload :swank :silent t)
(swank:create-server :port 4005 :dont-close t)
;; Keep running
(loop (sleep 3600))