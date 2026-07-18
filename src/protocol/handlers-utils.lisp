;;;; src/protocol/handlers-utils.lisp
;;;;
;;;; Utility functions for JSON-RPC handlers.
;;;;
;;;; This file contains:
;;;;   - Parameter validation functions
;;;;   - Unix time helper
;;;;   - Error response construction
;;;;
;;;; NOTE (cl-tron-mcp#2, "bug #8"): this file used to also define
;;;; cleanup-on-error, validate-list-param, a with-timeout macro, and a
;;;; timeout-error condition. Every caller of those four turned out to be
;;;; src/tools/handlers-tools.lisp (cl-tron-mcp/tools package) -- nothing
;;;; else in cl-tron-mcp/protocol used them -- so they were relocated to
;;;; src/tools/handlers-support.lisp, in the package that actually calls
;;;; them (cl-tron-mcp/tools cannot import from cl-tron-mcp/protocol
;;;; without a dependency cycle: protocol depends on tools, not vice
;;;; versa). validate-string-param and make-error-response stay here
;;;; (protocol's own handlers-prompts.lisp / handlers-resources.lisp /
;;;; this file's own logic still use them); handlers-support.lisp defines
;;;; independent, identical copies for tools' own use rather than
;;;; importing across the package boundary -- see that file's header
;;;; comment for why.

(in-package :cl-tron-mcp/protocol)

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
;;; Unix Time Helper
;;; ============================================================

(defun get-unix-time ()
  "Get current Unix timestamp (seconds since epoch)."
  #+sbcl
  (sb-ext:get-time-of-day)
  #+ccl
  (ccl:get-time-of-day)
  #+ecl
  (- (get-universal-time) 2208988800)
  #-(or sbcl ccl ecl)
  (- (get-universal-time) 2208988800))

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

(provide :cl-tron-mcp/protocol-handlers-utils)
