;;;; src/tools/logging-tools.lisp
;;;; Logging tool registrations

(in-package :cl-tron-mcp/tools)

(define-validated-tool "log_configure"
  "Configure logging level"
  :input-schema (list :level (list :enum (list "trace" "debug" "info" "warn" "error" "fatal")) :package "string" :appender "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/log-configure.md"
  :validation ((when level (validate-choice "level" level '("trace" "debug" "info" "warn" "error" "fatal")))
               (when package (validate-package-name "package" package))
               (when appender (validate-string "appender" appender)))
  :body (cl-tron-mcp/logging:log-configure :level level :package package :appender appender))

(define-validated-tool "log_info"
  "Log info message"
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/log-info.md"
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-info :message message :package package))

(define-validated-tool "log_debug"
  "Log debug message"
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/log-debug.md"
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-debug :message message :package package))

(define-validated-tool "log_warn"
  "Log warning message"
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/log-warn.md"
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-warn :message message :package package))

(define-validated-tool "log_error"
  "Log error message"
  :input-schema (list :message "string" :package "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/log-error.md"
  :validation ((validate-string "message" message :required t)
               (when package (validate-package-name "package" package)))
  :body (cl-tron-mcp/logging:log-error :message message :package package))