;;;; src/core/error-codes.lisp - Centralized error code system
;;;;
;;;; This module provides a centralized error code system to reduce
;;;; token usage by replacing verbose error messages with compact codes.
;;;;
;;;; Error codes are mapped to detailed information including:
;;;; - Short message (always included)
;;;; - Hint (lazy-loaded, only when requested)
;;;; - Setup instructions (lazy-loaded)
;;;; - Documentation URI (lazy-loaded)
;;;;
;;;; Usage:
;;;;   (make-error "REPL_NOT_CONNECTED" :details (list :tool "repl_eval"))
;;;;   (make-error-with-hint "MISSING_REQUIRED_PARAMETER" :details (list :param "code"))

(in-package :cl-tron-mcp/core)

;;; ============================================================
;;; Error Code Definitions
;;; ============================================================

(defparameter *error-codes*
  (list
   (list :code "REPL_NOT_CONNECTED"
         :message "Not connected to any REPL"
         :hint "Run repl_connect first. Example: repl_connect :port 4006"
         :setup "To start Swank in SBCL: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)"
         :documentation-uri "docs/errors/repl-not-connected.md")

   (list :code "REPL_ALREADY_CONNECTED"
         :message "Already connected to a REPL"
         :hint "Use repl_disconnect first if you want to connect to a different server"
         :setup nil
         :documentation-uri "docs/errors/repl-already-connected.md")

   (list :code "NREPL_NOT_SUPPORTED"
         :message "nrepl is no longer supported; use Swank"
         :hint "Use repl_connect :type :swank :port 4006 or start Swank: (ql:quickload :swank) (swank:create-server :port 4006)"
         :setup nil
         :documentation-uri "docs/errors/nrepl-not-supported.md")

   (list :code "REPL_DETECTION_FAILED"
         :message "Could not detect Swank"
         :hint "Ensure Swank server is running on the specified port"
         :setup "For Swank: (ql:quickload :swank) (swank:create-server :port 4006)"
         :documentation-uri "docs/errors/repl-detection-failed.md")

   (list :code "MISSING_REQUIRED_PARAMETER"
         :message "Missing required parameter"
         :hint "Provide a value for this parameter"
         :setup nil
         :documentation-uri "docs/errors/missing-required-parameter.md")

   (list :code "INVALID_STRING_PARAMETER"
         :message "Parameter must be a string"
         :hint "Ensure the parameter value is a string type"
         :setup nil
         :documentation-uri "docs/errors/invalid-string-parameter.md")

   (list :code "INVALID_LIST_PARAMETER"
         :message "Parameter must be a list"
         :hint "Ensure the parameter value is a list"
         :setup nil
         :documentation-uri "docs/errors/invalid-list-parameter.md")

   (list :code "INVALID_INTEGER_PARAMETER"
         :message "Parameter must be an integer"
         :hint "Ensure the parameter value is an integer"
         :setup nil
         :documentation-uri "docs/errors/invalid-integer-parameter.md")

   (list :code "PARAMETER_TOO_SHORT"
         :message "Parameter must be at least minimum length"
         :hint "Provide a longer value for this parameter"
         :setup nil
         :documentation-uri "docs/errors/parameter-too-short.md")

   (list :code "PARAMETER_TOO_SMALL"
         :message "Parameter must be >= minimum value"
         :hint "Provide a larger value for this parameter"
         :setup nil
         :documentation-uri "docs/errors/parameter-too-small.md")

   (list :code "SWANK_ALREADY_CONNECTED"
         :message "Already connected to Swank server"
         :hint "Use swank_disconnect first if you want to connect to a different server"
         :setup nil
         :documentation-uri "docs/errors/swank-already-connected.md")

   (list :code "SWANK_CONNECTION_FAILED"
         :message "Failed to connect to Swank"
         :hint "Ensure Swank server is running and accessible"
         :setup "Start Swank: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)"
         :documentation-uri "docs/errors/swank-connection-failed.md")

   (list :code "SWANK_NOT_CONNECTED"
         :message "Not connected to Swank server"
         :hint "Use swank_connect to establish a connection"
         :setup "Start Swank: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)"
         :documentation-uri "docs/errors/swank-not-connected.md")

   (list :code "REQUEST_NOT_FOUND"
         :message "Request not found"
         :hint "The request ID does not exist or has expired"
         :setup nil
         :documentation-uri "docs/errors/request-not-found.md")

   (list :code "REQUEST_TIMEOUT"
         :message "Request timeout"
         :hint "The operation took too long to complete"
         :setup nil
         :documentation-uri "docs/errors/request-timeout.md")

   (list :code "RECONNECTION_DISABLED"
         :message "Reconnection is disabled"
         :hint "Enable reconnection or manually reconnect"
         :setup nil
         :documentation-uri "docs/errors/reconnection-disabled.md")

   (list :code "MAX_RECONNECTION_ATTEMPTS"
         :message "Max reconnection attempts reached"
         :hint "Check server availability and try again later"
         :setup nil
         :documentation-uri "docs/errors/max-reconnection-attempts.md")

   (list :code "RECONNECTION_ERROR"
         :message "Reconnection error"
         :hint "Check server status and network connectivity"
         :setup nil
         :documentation-uri "docs/errors/reconnection-error.md")

   (list :code "INVALID_CODE_PARAMETER"
         :message "code is required and must be a non-empty string"
         :hint "Provide a Lisp expression as a string, e.g., :code \"(+ 1 2)\""
         :setup nil
         :documentation-uri "docs/errors/invalid-code-parameter.md")

   (list :code "INVALID_PACKAGE_PARAMETER"
         :message "package is required and must be a non-empty string"
         :hint "Provide a package name as a string, e.g., :package \"CL-USER\""
         :setup nil
         :documentation-uri "docs/errors/invalid-package-parameter.md")

   (list :code "INVALID_FILENAME_PARAMETER"
         :message "filename is required and must be a non-empty string"
         :hint "Provide a filename as a string, e.g., :filename \"my-file\""
         :setup nil
         :documentation-uri "docs/errors/invalid-filename-parameter.md")

   (list :code "INVALID_FRAME_INDEX"
         :message "frame-index must be a non-negative integer"
         :hint "Provide a valid frame index starting from 0"
         :setup nil
         :documentation-uri "docs/errors/invalid-frame-index.md")

   (list :code "NO_DEBUGGER_EVENT"
         :message "No debugger event available"
         :hint "Enter the debugger first by triggering an error or using a breakpoint"
         :setup nil
         :documentation-uri "docs/errors/no-debugger-event.md")

   (list :code "NOT_IN_STEPPER"
         :message "Not in stepper"
         :hint "Enter the stepper with (step ...) first"
         :setup nil
         :documentation-uri "docs/errors/not-in-stepper.md")

   (list :code "INTERRUPT_ERROR"
         :message "Interrupt error"
         :hint "The thread may not be interruptible or may have already completed"
         :setup nil
         :documentation-uri "docs/errors/interrupt-error.md")

   (list :code "INVALID_EXPRESSION_PARAMETER"
         :message "expression is required and must be a non-empty string"
         :hint "Provide an expression that evaluates to an object, e.g., :expression \"*package*\""
         :setup nil
         :documentation-uri "docs/errors/invalid-expression-parameter.md")

   (list :code "INVALID_SYMBOL_PARAMETER"
         :message "symbol is required and must be a non-empty string"
         :hint "Provide a symbol name, e.g., :symbol \"mapcar\""
         :setup nil
         :documentation-uri "docs/errors/invalid-symbol-parameter.md")

   (list :code "INVALID_PREFIX_PARAMETER"
         :message "prefix is required and must be a non-empty string"
         :hint "Provide a symbol prefix, e.g., :prefix \"mak\""
         :setup nil
         :documentation-uri "docs/errors/invalid-prefix-parameter.md")

   (list :code "NOT_A_FUNCTION"
         :message "Symbol is not a function"
         :hint "Provide a symbol that names a function"
         :setup nil
         :documentation-uri "docs/errors/not-a-function.md")

   (list :code "INVALID_BOOLEAN_PARAMETER"
         :message "approved must be a boolean or 'true'/'false' string"
         :hint "Provide a boolean value (t/nil) or the strings \"true\"/\"false\""
         :setup nil
         :documentation-uri "docs/errors/invalid-boolean-parameter.md")

   (list :code "APPROVAL_EXPIRED"
         :message "Approval expired or already used"
         :hint "Invoke the tool again to request a new approval"
         :setup nil
         :documentation-uri "docs/errors/approval-expired.md")

   (list :code "TOOL_EXECUTION_TIMEOUT"
         :message "Tool execution timeout"
         :hint "The tool took too long to complete. Consider optimizing the operation or increasing timeout"
         :setup nil
         :documentation-uri "docs/errors/tool-execution-timeout.md")

   (list :code "UNKNOWN_METHOD"
         :message "Unknown method"
         :hint "Check the method name and ensure it is supported by the server"
         :setup nil
         :documentation-uri "docs/errors/unknown-method.md")

   (list :code "JSON_PARSE_ERROR"
         :message "JSON parse error"
         :hint "Ensure the request is valid JSON format"
         :setup nil
         :documentation-uri "docs/errors/json-parse-error.md")

   (list :code "INTERNAL_ERROR"
         :message "Internal error"
         :hint "An unexpected error occurred. Check logs for details"
         :setup nil
         :documentation-uri "docs/errors/internal-error.md")

   (list :code "OBJECT_NOT_FOUND"
         :message "Object not found"
         :hint "The object ID does not exist in the object registry"
         :setup nil
         :documentation-uri "docs/errors/object-not-found.md")

   (list :code "CLASS_NOT_FOUND"
         :message "Class not found"
         :hint "The class name does not exist"
         :setup nil
         :documentation-uri "docs/errors/class-not-found.md")

   (list :code "PACKAGE_NOT_FOUND"
         :message "Package not found"
         :hint "The package name does not exist"
         :setup nil
         :documentation-uri "docs/errors/package-not-found.md"))

  "Central registry of error codes with their associated information.")

;;; ============================================================
;;; Error Code Lookup
;;; ============================================================

(defun get-error-info (code)
  "Retrieve full error information for a given error code.
Returns a plist with :code, :message, :hint, :setup, :documentation-uri.
Returns nil if code is not found."
  (find code *error-codes* :key (lambda (entry) (getf entry :code)) :test #'string=))

(defun get-error-message (code)
  "Get the short message for an error code."
  (let ((info (get-error-info code)))
    (when info (getf info :message))))

(defun get-error-hint (code)
  "Get the hint for an error code (lazy-loaded)."
  (let ((info (get-error-info code)))
    (when info (getf info :hint))))

(defun get-error-setup (code)
  "Get the setup instructions for an error code (lazy-loaded)."
  (let ((info (get-error-info code)))
    (when info (getf info :setup))))

(defun get-error-documentation-uri (code)
  "Get the documentation URI for an error code (lazy-loaded)."
  (let ((info (get-error-info code)))
    (when info (getf info :documentation-uri))))

;;; ============================================================
;;; Error Factory Functions
;;; ============================================================

(defun make-error (code &key details)
  "Create a minimal error response with just the code and short message.
DETAILS is an optional plist with additional context."
  (let ((message (get-error-message code)))
    (if message
        (list* :error t :code code :message message details)
        (list :error t :code code :message (format nil "Unknown error code: ~a" code)))))

(defun make-error-with-hint (code &key details)
  "Create an error response with code, message, and hint.
DETAILS is an optional plist with additional context."
  (let ((message (get-error-message code))
        (hint (get-error-hint code)))
    (if message
        (list* :error t :code code :message message :hint hint details)
        (list :error t :code code :message (format nil "Unknown error code: ~a" code)))))

(defun make-error-full (code &key details)
  "Create a complete error response with all available information.
DETAILS is an optional plist with additional context."
  (let ((message (get-error-message code))
        (hint (get-error-hint code))
        (setup (get-error-setup code))
        (docs (get-error-documentation-uri code)))
    (if message
        (let ((base (list* :error t :code code :message message :hint hint details)))
          (list* base
                 (when setup (list :setup setup))
                 (when docs (list :docs docs))))
        (list :error t :code code :message (format nil "Unknown error code: ~a" code)))))

(provide :cl-tron-mcp/core/error-codes)