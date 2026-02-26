;;;; src/tools/validation.lisp
;;;; Input validation utilities for tool handlers

(in-package :cl-tron-mcp/tools)

;;; Validation errors
(define-condition validation-error (error)
  ((parameter :initarg :parameter :reader validation-error-parameter)
   (message :initarg :message :reader validation-error-message))
  (:report (lambda (condition stream)
             (format stream "Validation error for parameter ~a: ~a"
                     (validation-error-parameter condition)
                     (validation-error-message condition)))))

;;; Validation functions
(defun validate-required (name value)
  "Validate that VALUE is not nil.
Raises validation-error if validation fails."
  (when (null value)
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  value)

(defun validate-string (name value &key required (min-length 0) (max-length nil))
  "Validate that VALUE is a string with optional length constraints.
Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (stringp value)))
    (error 'validation-error
           :parameter name
           :message (format nil "Expected string, got ~a" (type-of value))))
  (when (and value (stringp value))
    (when (< (length value) min-length)
      (error 'validation-error
             :parameter name
             :message (format nil "String too short (minimum ~d characters)" min-length)))
    (when (and max-length (> (length value) max-length))
      (error 'validation-error
             :parameter name
             :message (format nil "String too long (maximum ~d characters)" max-length))))
  value)

(defun validate-integer (name value &key required (min nil) (max nil))
  "Validate that VALUE is an integer with optional range constraints.
Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (integerp value)))
    (error 'validation-error
           :parameter name
           :message (format nil "Expected integer, got ~a" (type-of value))))
  (when (and value (integerp value))
    (when (and min (< value min))
      (error 'validation-error
             :parameter name
             :message (format nil "Value too small (minimum ~d)" min)))
    (when (and max (> value max))
      (error 'validation-error
             :parameter name
             :message (format nil "Value too large (maximum ~d)" max))))
  value)

(defun validate-boolean (name value &key required)
  "Validate that VALUE is a boolean (t or nil).
Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (or (eq value t) (eq value nil))))
    (error 'validation-error
           :parameter name
           :message (format nil "Expected boolean, got ~a" (type-of value))))
  value)

(defun validate-choice (name value choices &key required)
  "Validate that VALUE is one of the CHOICES.
Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (member value choices :test #'equal)))
    (error 'validation-error
           :parameter name
           :message (format nil "Invalid value ~a, must be one of: ~a" value choices)))
  value)

(defun validate-object-id (name value &key required)
  "Validate that VALUE is a valid object ID (string or integer).
Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (or (stringp value) (integerp value))))
    (error 'validation-error
           :parameter name
           :message (format nil "Expected object ID (string or integer), got ~a" (type-of value))))
  value)

(defun validate-symbol-name (name value &key required)
  "Validate that VALUE is a valid symbol name (string).
Raises validation-error if validation fails."
  (validate-string name value :required required :min-length 1)
  (when (and value (stringp value))
    (unless (cl-ppcre:scan "^[a-zA-Z0-9-+*/<>=!&$%_:]+$" value)
      (error 'validation-error
             :parameter name
             :message "Invalid symbol name")))
  value)

(defun validate-package-name (name value &key required)
  "Validate that VALUE is a valid package name (string).
Raises validation-error if validation fails."
  (validate-string name value :required required :min-length 1)
  (when (and value (stringp value))
    (unless (cl-ppcre:scan "^[a-zA-Z0-9-]+$" value)
      (error 'validation-error
             :parameter name
             :message "Invalid package name")))
  value)

(defmacro with-validation ((&rest validations) &body body)
  "Execute BODY with input validation.
VALIDATIONS is a list of (function-name parameter-name &rest args)."
  `(handler-case
       (progn
         ,@(loop for (func param . args) in validations
                 collect `(,func ,param ,@args))
         ,@body)
     (validation-error (e)
       (list :error t
             :message (format nil "Validation error: ~a" (validation-error-message e))
             :parameter (validation-error-parameter e)))))