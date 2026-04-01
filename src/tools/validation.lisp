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

(defun validate-url (name value &key required)
  "Validate that VALUE is a well-formed URL (http, https, ftp, or file scheme).
Raises validation-error if validation fails."
  (validate-string name value :required required :min-length 1)
  (when (and value (stringp value))
    (unless (cl-ppcre:scan
             "^(https?|ftp|file)://[^\\s]+"
             value)
      (error 'validation-error
             :parameter name
             :message "Invalid URL (expected http://, https://, ftp://, or file://)")))
  value)

(defun validate-uri (name value &key required)
  "Validate that VALUE is a non-empty URI string (any scheme).
Raises validation-error if validation fails."
  (validate-string name value :required required :min-length 1)
  (when (and value (stringp value))
    (unless (cl-ppcre:scan "^[a-zA-Z][a-zA-Z0-9+\\-.]*:" value)
      (error 'validation-error
             :parameter name
             :message "Invalid URI (must have a scheme like http:, file:, etc.)")))
  value)

(defun validate-list (name value &key required (min-length 0) max-length element-validator)
  "Validate that VALUE is a list with optional length and per-element constraints.
ELEMENT-VALIDATOR, if provided, is called as (element-validator element-name element-value)
for each element. Raises validation-error if validation fails."
  (when (and required (null value))
    (error 'validation-error
           :parameter name
           :message "Required parameter is missing"))
  (when (and value (not (listp value)))
    (error 'validation-error
           :parameter name
           :message (format nil "Expected list, got ~a" (type-of value))))
  (when (and value (listp value))
    (when (< (length value) min-length)
      (error 'validation-error
             :parameter name
             :message (format nil "List too short (minimum ~d elements)" min-length)))
    (when (and max-length (> (length value) max-length))
      (error 'validation-error
             :parameter name
             :message (format nil "List too long (maximum ~d elements)" max-length)))
    (when element-validator
      (loop for i from 0
            for element in value
            do (funcall element-validator
                        (format nil "~a[~d]" name i)
                        element))))
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

;;; ============================================================
;;; Custom Validator Registry
;;; ============================================================

(defvar *custom-validators* (make-hash-table :test 'equal)
  "Registry of custom validators keyed by name (string).
Each entry is a function (name value &rest args) -> value or validation-error.")

(defun register-validator (name validator-fn)
  "Register a custom validator under NAME.
VALIDATOR-FN is a function (parameter-name value &rest keyword-args) that
should return VALUE if valid or signal VALIDATION-ERROR if not."
  (check-type name string)
  (check-type validator-fn function)
  (setf (gethash name *custom-validators*) validator-fn)
  name)

(defun find-validator (name)
  "Return the validator function registered under NAME, or NIL."
  (gethash name *custom-validators*))

(defun list-validators ()
  "Return a list of all registered custom validator names."
  (let (names)
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *custom-validators*)
    (sort names #'string<)))

(defun unregister-validator (name)
  "Remove the validator registered under NAME. Returns T if it existed."
  (when (gethash name *custom-validators*)
    (remhash name *custom-validators*)
    t))

(defun validate-with (validator-name parameter-name value &rest args)
  "Apply the custom validator named VALIDATOR-NAME to VALUE.
Raises VALIDATION-ERROR if the validator is not registered or validation fails."
  (let ((fn (find-validator validator-name)))
    (unless fn
      (error 'validation-error
             :parameter parameter-name
             :message (format nil "Unknown validator: ~a" validator-name)))
    (apply fn parameter-name value args)))

(defmacro define-validator (name (param-name value-var &rest lambda-list) &body body)
  "Define and register a custom validator named NAME.
PARAM-NAME and VALUE-VAR are bound to the parameter name and value during validation.
LAMBDA-LIST receives optional keyword arguments passed to VALIDATE-WITH.
The body should return the (possibly coerced) value or signal VALIDATION-ERROR.

Example:
  (define-validator \"positive-integer\" (name val)
    (validate-integer name val :required t :min 1)
    val)"
  `(register-validator ,name
                       (lambda (,param-name ,value-var ,@lambda-list)
                         ,@body)))