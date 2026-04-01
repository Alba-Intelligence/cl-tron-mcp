;;;; src/sbcl/debug-internals.lisp

(in-package :cl-tron-mcp/sbcl)

(defvar *object-registry* (make-hash-table :test 'eql))
(defvar *object-registry-timestamps* (make-hash-table :test 'eql))
(defvar *next-object-id* 1)
(defvar *registry-lock* (bt:make-lock "object-registry"))
(defvar *object-registry-max-age* 3600
  "Maximum age in seconds for object registry entries (default: 1 hour).")
(defvar *object-registry-max-size* 1000
  "Maximum number of entries in object registry before forced eviction.")

(defun register-object (object)
  "Register object and return ID."
  (bt:with-lock-held (*registry-lock*)
    (let ((id *next-object-id*)
          (now (get-universal-time)))
      (setf (gethash id *object-registry*) object)
      (setf (gethash id *object-registry-timestamps*) now)
      (incf *next-object-id*)
      id)))

(defun lookup-object (id)
  "Lookup object by ID, updating its timestamp."
  (bt:with-lock-held (*registry-lock*)
    (let ((obj (gethash id *object-registry*)))
      (when obj
        (setf (gethash id *object-registry-timestamps*) (get-universal-time)))
      obj)))

(defun get-object-id (object)
  "Get existing ID or register new object."
  (bt:with-lock-held (*registry-lock*)
    (maphash (lambda (k v)
               (when (eq v object)
                 (return-from get-object-id k)))
             *object-registry*)
    (let ((id *next-object-id*)
          (now (get-universal-time)))
      (setf (gethash id *object-registry*) object)
      (setf (gethash id *object-registry-timestamps*) now)
      (incf *next-object-id*)
      id)))

(defun evict-expired-objects ()
  "Remove object registry entries older than *object-registry-max-age* seconds.
Returns the number of entries evicted."
  (bt:with-lock-held (*registry-lock*)
    (let ((cutoff (- (get-universal-time) *object-registry-max-age*))
          (evicted 0))
      (maphash (lambda (id timestamp)
                 (when (< timestamp cutoff)
                   (remhash id *object-registry*)
                   (remhash id *object-registry-timestamps*)
                   (incf evicted)))
               *object-registry-timestamps*)
      evicted)))

(defun evict-lru-objects (&optional (target-size (floor *object-registry-max-size* 2)))
  "Evict least-recently-used entries until registry size is at or below TARGET-SIZE.
Returns the number of entries evicted."
  (bt:with-lock-held (*registry-lock*)
    (let ((current-size (hash-table-count *object-registry*)))
      (when (> current-size target-size)
        (let ((entries (sort (loop for id being the hash-keys of *object-registry-timestamps*
                                   for ts = (gethash id *object-registry-timestamps*)
                                   collect (cons ts id))
                             #'< :key #'car))
              (to-remove (- current-size target-size)))
          (loop for i from 0 below to-remove
                for (nil . id) in entries
                do (remhash id *object-registry*)
                   (remhash id *object-registry-timestamps*)
                finally (return i)))))))

(defun maybe-evict-objects ()
  "Evict expired objects; if registry is still too large, also evict LRU entries."
  (evict-expired-objects)
  (when (>= (hash-table-count *object-registry*) *object-registry-max-size*)
    (evict-lru-objects)))

(defun clear-object-registry ()
  "Clear all registered objects."
  (bt:with-lock-held (*registry-lock*)
    (clrhash *object-registry*)
    (clrhash *object-registry-timestamps*)
    (setq *next-object-id* 1)))
