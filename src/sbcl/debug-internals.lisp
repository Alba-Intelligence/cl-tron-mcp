;;;; src/sbcl/debug-internals.lisp

(in-package :cl-tron-mcp/sbcl)

(defvar *object-registry* (make-hash-table :test 'eql))
(defvar *next-object-id* 1)
(defvar *registry-lock* (bt:make-lock "object-registry"))

(defun register-object (object)
  "Register object and return ID."
  (bt:with-lock-held (*registry-lock*)
    (let ((id *next-object-id*))
      (setf (gethash id *object-registry*) object)
      (incf *next-object-id*)
      id)))

(defun lookup-object (id)
  "Lookup object by ID."
  (bt:with-lock-held (*registry-lock*)
    (gethash id *object-registry*)))

(defun get-object-id (object)
  "Get existing ID or register new object."
  (bt:with-lock-held (*registry-lock*)
    (maphash (lambda (k v)
               (when (eq v object)
                 (return-from get-object-id k)))
             *object-registry*)
    (let ((id *next-object-id*))
      (setf (gethash id *object-registry*) object)
      (incf *next-object-id*)
      id)))

(defun clear-object-registry ()
  "Clear all registered objects."
  (bt:with-lock-held (*registry-lock*)
    (clrhash *object-registry*)
    (setq *next-object-id* 1)))
