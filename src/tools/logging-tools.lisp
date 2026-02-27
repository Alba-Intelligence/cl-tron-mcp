;;;; src/tools/logging-tools.lisp
;;;; Logging tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "log_configure"
  "Configure logging level for a package. Levels: trace, debug, info, warn, error, fatal. Use to control log verbosity."
  :input-schema (list :level (list :enum (list "trace" "debug" "info" "warn" "error" "fatal")) :package "string" :appender "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when level (validate-choice "level" level '("trace" "debug" "info" "warn" "error" "fatal")))
               (when package (validate-package-name "package" package))
               (when appender (validate-string "appender" appender)))
  :body (cl-tron-mcp/logging:log-configure :level level :package package :appender appender))

(define-validated-tool "log_info"
  "Log an info message. Use for general information about program execution."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-info :message message :package package))

(define-validated-tool "log_debug"
  "Log a debug message. Use for detailed debugging information."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-debug :message message :package package))

(define-validated-tool "log_warn"
  "Log a warning message. Use for non-fatal issues that should be noted."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-warn :message message :package package))

(define-validated-tool "log_error"
  "Log an error message. Use for errors that don't crash the program."
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-error :message message :package package))