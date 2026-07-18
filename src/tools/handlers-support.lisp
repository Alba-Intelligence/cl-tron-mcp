;;;; src/tools/handlers-support.lisp
;;;;
;;;; Tool-call execution support: parameter validation, error-response
;;;; construction, error cleanup, and timeout state -- used by
;;;; src/tools/handlers-tools.lisp (handle-tool-call and friends).
;;;;
;;;; Why this file exists (cl-tron-mcp#2, "bug #8"):
;;;;
;;;; handlers-tools.lisp lives in the cl-tron-mcp/tools package/system
;;;; because it implements handle-tools-list / handle-tool-call /
;;;; handle-approval-respond, which cl-tron-mcp/protocol imports
;;;; (cl-tron-mcp/protocol depends on cl-tron-mcp/tools, never the
;;;; reverse -- see cl-tron-mcp.asd). It used to call
;;;; validate-string-param, validate-list-param, make-error-response,
;;;; cleanup-on-error, and reference *request-lock* / *pending-requests*
;;;; / *default-tool-timeout* / the timeout-error condition as bare
;;;; symbols, on the assumption they'd resolve to the definitions in
;;;; src/protocol/handlers-utils.lisp and src/protocol/handlers.lisp.
;;;; They do not: cl-tron-mcp/tools cannot import from cl-tron-mcp/protocol
;;;; without introducing a dependency cycle. Every unqualified reference
;;;; instead silently interned a distinct, never-defined symbol in the
;;;; cl-tron-mcp/tools package, so every tools/call request threw
;;;; "...VALIDATE-STRING-PARAM undefined" before any tool ran.
;;;;
;;;; Fix: relocate this tool-call-execution machinery to the package
;;;; that actually uses it. Checked every other reader in
;;;; cl-tron-mcp/protocol before moving anything:
;;;;   - cleanup-on-error, validate-list-param, the timeout-error
;;;;     condition, *request-lock*, *pending-requests*, and
;;;;     *default-tool-timeout* had exactly one caller each in the whole
;;;;     repo: handlers-tools.lisp. Moved outright (deleted from
;;;;     src/protocol/handlers-utils.lisp / handlers.lisp, defined fresh
;;;;     here) -- no duplication, no import-back needed.
;;;;   - validate-string-param is also called from
;;;;     src/protocol/handlers-prompts.lisp and handlers-resources.lisp,
;;;;     so protocol keeps its own copy; this file defines an
;;;;     independent, identical copy for tools' own use rather than
;;;;     importing across the package boundary.
;;;;   - make-error-response is defined independently here too, rather
;;;;     than shared/imported, to sidestep a separate pre-existing wart:
;;;;     cl-tron-mcp/protocol defines make-error-response TWICE in the
;;;;     same package (messages.lisp's 4-arg version, silently shadowed
;;;;     by handlers-utils.lisp's richer 3-arg version loading after it).
;;;;     Importing that symbol here would make this copy vulnerable to
;;;;     whichever definition wins protocol's internal load-order race.
;;;;     Not fixed here -- out of scope for bug #8; flagged as a
;;;;     follow-up in the task report.

(in-package :cl-tron-mcp/tools)

;;; ============================================================
;;; Tool-call execution state
;;; ============================================================

(defvar *default-tool-timeout* 30
  "Default timeout for tool execution in seconds.")

(defvar *pending-requests* (make-hash-table :test 'eql)
  "Hash table tracking pending tool-call requests for cleanup.")

(defvar *request-lock* (bordeaux-threads:make-lock "pending-tool-requests")
  "Lock for synchronizing access to *pending-requests*.")

;;; ============================================================
;;; Timeout condition
;;; ============================================================

(define-condition timeout-error
    (error)
  ((message :initarg :message
            :reader timeout-error-message))
  (:report (lambda (condition stream)
             (format stream
                     "Timeout: ~a"
                     (timeout-error-message condition)))))

;;; ============================================================
;;; Parameter validation
;;; ============================================================

(defun validate-string-param (param-name value
                              &key
                                required
                                (min-length 1))
  "Validate a string parameter.
Returns T if valid, otherwise returns an error response plist."
  (progn
    (when (and required
               (null value))
      (return-from validate-string-param
        (list :valid nil
              :error (list :code "MISSING_REQUIRED_PARAMETER"
                           :message (format nil "Missing required parameter: ~a"
                                            param-name)
                           :param param-name))))
    (when (and value
               (not (stringp value)))
      (return-from validate-string-param
        (list :valid nil
              :error (list :code "INVALID_STRING_PARAMETER"
                           :message (format nil "Parameter ~a must be a string"
                                            param-name)
                           :param param-name))))
    (when (and value
               (stringp value)
               (< (length value) min-length))
      (return-from validate-string-param
        (list :valid nil
              :error (list :code "PARAMETER_TOO_SHORT"
                           :message (format nil "Parameter ~a must be at least ~d characters"
                                            param-name min-length)
                           :param param-name
                           :min-length min-length))))
    (list :valid t)))

(defun validate-list-param (param-name value &key required)
  "Validate a list parameter.
Returns T if valid, otherwise returns an error response plist."
  (progn
    (when (and required
               (null value))
      (return-from validate-list-param
        (list :valid nil
              :error (list :code "MISSING_REQUIRED_PARAMETER"
                           :message (format nil "Missing required parameter: ~a"
                                            param-name)
                           :param param-name))))
    (when (and value
               (not (listp value)))
      (return-from validate-list-param
        (list :valid nil
              :error (list :code "INVALID_LIST_PARAMETER"
                           :message (format nil "Parameter ~a must be a list"
                                            param-name)
                           :param param-name))))
    (list :valid t)))

;;; ============================================================
;;; Error response construction
;;; ============================================================

(defun make-error-response (id code message)
  "Create JSON-RPC error response.
CODE should be a standard JSON-RPC error code:
   -32600 : Invalid Request
   -32601 : Method Not Found
   -32602 : Invalid Params
   -32603 : Internal Error
   -32000 to -32099 : Server-defined errors
MESSAGE can be a string or a plist with :code, :message, :hint, etc."
  (let ((error-data (if (listp message)
                        message
                        (list :message message))))
    (let ((error-object (list :|code| code
                              :|message| (getf error-data :message))))
      (when (getf error-data :code)
        (setf error-object
              (append error-object (list :|errorCode| (getf error-data :code)))))
      (when (getf error-data :hint)
        (setf error-object
              (append error-object (list :|hint| (getf error-data :hint)))))
      (when (getf error-data :param)
        (setf error-object
              (append error-object (list :|param| (getf error-data :param)))))
      (when (getf error-data :min-length)
        (setf error-object
              (append error-object (list :|minLength| (getf error-data :min-length)))))
      (when (getf error-data :min)
        (setf error-object
              (append error-object (list :|min| (getf error-data :min)))))
      (cl-tron-mcp/json-compat:to-json (list :|jsonrpc| "2.0"
                              :|id| id
                              :|error| error-object)))))

;;; ============================================================
;;; Error Recovery and Cleanup
;;; ============================================================

(defun cleanup-on-error (error-context &optional
                                         (error-condition nil))
  "Perform cleanup when a tool-level error occurs.
ERROR-CONTEXT is a string describing what operation failed.
ERROR-CONDITION is the actual condition object (optional).

FD-009 bug #10 (connection-state robustness): this MUST NOT drop, clear, or
corrupt the live Swank connection.  A tool-level failure — a validation
error, a mis-wired keyword, a rejected/invalid rex — is not a connection
failure.  The previous version called SWANK-DISCONNECT on ANY tool error,
tearing down the socket and reader threads (and clearing *swank-connected*)
while the tools layer's own *repl-connected* flag stayed T.  That left the
connection in an inconsistent state: repl_eval reported \"Not connected to
Swank server\" while repl_connect reported \"Already connected\", wedging a
long-lived agent loop after a single bad request.  Cleanup is now localized:
log the error and drop any stale request-correlation entries; the connection
stays usable so the very next request succeeds."
  (handler-case (progn
                  (cl-tron-mcp/logging:log-error (format nil
                                                         "Error in ~a: ~a"
                                                         error-context
                                                         (if error-condition
                                                             (princ-to-string error-condition)
                                                             "unknown")))
                  ;; Clear stale request-correlation entries only.  Do NOT
                  ;; disconnect: the connection must survive tool-level errors.
                  (bordeaux-threads:with-lock-held (*request-lock*)
                    (clrhash *pending-requests*))
                  (cl-tron-mcp/logging:log-info (format nil "Cleanup completed for error in ~a (connection preserved)"
                                                        error-context)))
    (error (e)
      (cl-tron-mcp/logging:log-error (format nil "Error during cleanup: ~a" e)))))

(provide :cl-tron-mcp/tools-handlers-support)
