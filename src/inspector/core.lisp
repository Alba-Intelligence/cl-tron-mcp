;;;; src/inspector/core.lisp

(in-package :cl-tron-mcp/inspector)

(defun inspect-object (object-id &key (max-depth 3) (max-elements 100))
  "Inspect object by ID."
  (let ((object (cl-tron-mcp/sbcl:lookup-object object-id)))
    (unless object
      (return-from inspect-object
        (list :error t
              :message (format nil "Object ~a not found" object-id))))
    (inspect-object-recursive object 0 max-depth max-elements)))

(defun inspect-object-recursive (object depth max-depth max-elements)
  "Recursively inspect object."
  (when (>= depth max-depth)
    (return-from inspect-object-recursive
      (format nil "~a" object)))
  (typecase object
    (cons (inspect-cons object depth max-depth max-elements))
    (array (inspect-array object depth max-depth max-elements))
    (hash-table (inspect-hash-table object depth max-depth max-elements))
    (standard-object (inspect-standard-object object depth max-depth max-elements))
    (t (format nil "~a" object))))

(defun inspect-cons (cons depth max-depth max-elements)
  "Inspect cons cell."
  (let ((elements (loop for c = cons then (cdr c)
                       for i below max-elements
                       while c
                       collect (inspect-object-recursive (car c) (1+ depth) max-depth max-elements))))
    (list :type "cons"
          :elements elements)))

(defun inspect-array (array depth max-depth max-elements)
  "Inspect array."
  (let ((elements (loop for i below (min (length array) max-elements)
                        collect (inspect-object-recursive (aref array i) (1+ depth) max-depth max-elements))))
    (list :type "array"
          :dimensions (array-dimensions array)
          :elements elements)))

(defun inspect-hash-table (hash-table depth max-depth max-elements)
  "Inspect hash table."
  (let ((entries (loop for i = 0 then (1+ i)
                       for key being the hash-keys of hash-table
                       using (hash-value value)
                       while (< i max-elements)
                       collect (list :key (inspect-object-recursive key depth max-depth max-elements)
                                    :value (inspect-object-recursive value depth max-depth max-elements)))))
    (list :type "hash-table"
          :count (hash-table-count hash-table)
          :entries entries)))

(defun inspect-standard-object (object depth max-depth max-elements)
  "Inspect CLOS instance."
  (list :type "standard-object"
        :description (format nil "~a" object)))

(defun inspect-slot (object-id slot-name &optional value)
  "Get or set slot value."
  (let ((object (cl-tron-mcp/sbcl:lookup-object object-id)))
    (unless object
      (return-from inspect-slot
        (list :error t
              :message (format nil "Object ~a not found" object-id))))
    (when value
      (setf (slot-value object (intern (string-upcase slot-name) :keyword))
            value))
    (list :slot slot-name
          :value (format nil "~a"
                        (ignore-errors
                          (slot-value object (intern (string-upcase slot-name) :keyword)))))))

(defun inspect-class (class-name)
  "Inspect class definition."
  (list :name class-name
        :message "Class inspection placeholder"))

(defun inspect-function (symbol-name)
  "Inspect function definition."
  (list :symbol symbol-name
        :message "Function inspection placeholder"))

(defun inspect-package (package-name)
  "Inspect package contents."
  (let ((package (find-package (string-upcase package-name))))
    (unless package
      (return-from inspect-package
        (list :error t
              :message (format nil "Package ~a not found" package-name))))
    (let ((count 0))
      (do-external-symbols (s package)
        (declare (ignore s))
        (incf count))
      (list :name (package-name package)
            :nicknames (package-nicknames package)
            :exports-count count))))
