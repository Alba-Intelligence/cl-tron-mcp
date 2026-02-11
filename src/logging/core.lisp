;;;; src/logging/core.lisp

(in-package :cl-tron-mcp/logging)

(defvar *log-config* (list :level :info :package nil :appender :console))

(defun log-configure (&key level package appender)
  "Configure logging for a package or globally.
   LEVEL: :trace, :debug, :info, :warn, :error, :fatal
   PACKAGE: package name string (nil for global)
   APPENDER: :console (default)"
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (when level
          (setq *log-config* (list :level level :package package :appender appender))
          (if package
              (let ((pkg (find-package (string-upcase package))))
                (when pkg
                  (log:config pkg level)))
            (log:config level)))
        (list :success t
              :config *log-config*))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun log-level ()
  "Get current log level."
  (getf *log-config* :level :info))

(defun log-info (message &key package)
  "Log info message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (let ((pkg (find-package (string-upcase package))))
              (when pkg (log:info pkg message)))
          (log:info message))
        (list :logged t :message message :level :info))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-debug (message &key package)
  "Log debug message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (let ((pkg (find-package (string-upcase package))))
              (when pkg (log:debug pkg message)))
          (log:debug message))
        (list :logged t :message message :level :debug))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-warn (message &key package)
  "Log warning message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (let ((pkg (find-package (string-upcase package))))
              (when pkg (log:warn pkg message)))
          (log:warn message))
        (list :logged t :message message :level :warn))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun log-error (message &key package)
  "Log error message."
  (handler-case
      (progn
        #+quicklisp (ql:quickload :log4cl :silent t)
        (if package
            (let ((pkg (find-package (string-upcase package))))
              (when pkg (log:error pkg message)))
          (log:error message))
        (list :logged t :message message :level :error))
    (error (e)
      (list :error t :message (princ-to-string e)))))

(defun get-log-config ()
  "Get current logging configuration."
  *log-config*)
