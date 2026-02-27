;;;; src/protocol/handlers-utils.lisp
;;;;
;;;; Utility functions for JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - Error recovery and cleanup functions
;;;;   - Parameter validation functions
;;;;   - Timeout handling
;;;;   - Error response construction

(in-package :cl-tron-mcp/protocol)

;;; ============================================================
;;; Error Recovery and Cleanup
;;; ============================================================

(defun cleanup-on-error (error-context &optional
                                       (error-condition nil))
  "Perform cleanup when an error occurs.
ERROR-CONTEXT is a string describing what operation failed.
ERROR-CONDITION is the actual condition object (optional)."
  (handler-case (progn
                  (cl-tron-mcp/logging:log-error (format nil
                                                         "Error in ~a: ~a"
                                                         error-context
                                                         (if error-condition
                                                             (princ-to-string error-condition)
                                                           "unknown")))
                  ;; Disconnect from Swank if connected
                  (when (cl-tron-mcp/swank:swank-connected-p)
                    (cl-tron-mcp/logging:log-info (format nil "Disconnecting from Swank due to error in ~a"
                                                          error-context))
                    (cl-tron-mcp/swank:swank-disconnect))
                  ;; Clear pending requests
                  (bordeaux-threads:with-lock-held (*request-lock*)
                                                   (clrhash *pending-requests*))
                  (cl-tron-mcp/logging:log-info (format nil "Cleanup completed for error in ~a"
                                                        error-context)))
    (error (e)
           (cl-tron-mcp/logging:log-error (format nil "Error during cleanup: ~a" e)))))

;;; ============================================================
;;; Parameter Validation
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

(defun validate-integer-param (param-name value
                                           &key
                                           required
                                           (min 0))
  "Validate an integer parameter.
Returns T if valid, otherwise returns an error response plist."
  (progn
    (when (and required
               (null value))
      (return-from validate-integer-param
        (list :valid nil
              :error (list :code "MISSING_REQUIRED_PARAMETER"
                           :message (format nil "Missing required parameter: ~a"
                                            param-name)
                           :param param-name))))
    (when (and value
               (not (integerp value)))
      (return-from validate-integer-param
        (list :valid nil
              :error (list :code "INVALID_INTEGER_PARAMETER"
                           :message (format nil "Parameter ~a must be an integer"
                                            param-name)
                           :param param-name))))
    (when (and value
               (integerp value)
               (< value min))
      (return-from validate-integer-param
        (list :valid nil
              :error (list :code "PARAMETER_TOO_SMALL"
                           :message (format nil "Parameter ~a must be >= ~d" param-name
                                            min)
                           :param param-name
                           :min min))))
    (list :valid t)))

(defun validate-boolean-param (param-name value &key required)
  "Validate a boolean parameter.
Returns T if valid, otherwise returns an error response plist."
  (progn
    (when (and required
               (null value))
      (return-from validate-boolean-param
        (list :valid nil
              :error (list :code "MISSING_REQUIRED_PARAMETER"
                           :message (format nil "Missing required parameter: ~a"
                                            param-name)
                           :param param-name))))
    (when (and value
               (not (or (eq value t)
                        (eq value nil)
                        (and (stringp value)
                             (or (string= value "true")
                                 (string= value "false"))))))
      (return-from validate-boolean-param
        (list :valid nil
              :error (list :code "INVALID_BOOLEAN_PARAMETER"
                           :message (format nil "Parameter ~a must be a boolean or 'true'/'false' string"
                                            param-name)
                           :param param-name))))
    (list :valid t)))

;;; ============================================================
;;; Timeout Handling
;;; ============================================================

(defmacro with-timeout ((seconds) &body
                         body)
  "Execute BODY with a timeout of SECONDS seconds.
If timeout occurs, a TIMEOUT-ERROR condition is signaled."
  (let ((timeout-tag (gensym "TIMEOUT-TAG-")))
    `(handler-case (let ((start-time (get-universal-time)))
                     (unwind-protect
                         (progn ,@body)
                       (when (> (- (get-universal-time)
                                   start-time) ,seconds)
                         (error 'timeout-error :message "Operation timed out"))))
       (timeout-error (e)
                      (declare (ignore e))
                      (error 'timeout-error
                             :message (format nil "Operation timed out after ~d seconds"
                                              ,seconds))))))

(define-condition timeout-error
                    (error)
                    ((message :initarg :message
                              :reader timeout-error-message))
                    (:report (lambda (condition stream)
                               (format stream
                                       "Timeout: ~a"
                                       (timeout-error-message condition)))))

(defun get-unix-time ()
  "Get current Unix timestamp (seconds since epoch)."
  #+sbcl
  (sb-ext:get-time-of-day)
  #+ccl
  (ccl:get-time-of-day)
  #-(or sbcl ccl)
  (- (/ (get-universal-time)
        86400)
     2))

;;; ============================================================
;;; Error Response Construction
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
    (jonathan:to-json (list :|jsonrpc| "2.0"
                            :|id| id
                            :|error| (list* :|code| code
                                            (list :|message| (getf error-data :message))
                                            (when (getf error-data :code)
                                              (list :|errorCode| (getf error-data :code)))
                                            (when (getf error-data :hint)
                                              (list :|hint| (getf error-data :hint)))
                                            (when (getf error-data :param)
                                              (list :|param| (getf error-data :param)))
                                            (when (getf error-data :min-length)
                                              (list :|minLength| (getf error-data :min-length)))
                                            (when (getf error-data :min)
                                              (list :|min| (getf error-data :min))))))))

(provide :cl-tron-mcp/protocol-handlers-utils)
