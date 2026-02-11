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
    (let ((slot-symbol (intern (string-upcase slot-name) :keyword)))
      (when value
        (setf (slot-value object slot-symbol) value))
      (list :slot slot-name
            :value (format nil "~a"
                          (ignore-errors
                            (slot-value object slot-symbol)))))))

(defun inspect-class (class-name)
  "Inspect class definition using CLOS MOP."
  (let ((class (find-class (intern (string-upcase class-name) :cl) nil)))
    (unless class
      (return-from inspect-class
        (list :error t
              :message (format nil "Class ~a not found" class-name))))
    (list :name (class-name class)
          :direct-superclasses (mapcar #'class-name (class-direct-superclasses class))
          :direct-slots (mapcar (lambda (s)
                                  (list :name (slot-definition-name s)))
                                (class-direct-slots class))
                     :precedence-list (mapcar #'class-name (class-precedence-list class)))))

(defun inspect-function (symbol-name)
  "Inspect function definition."
  (let ((symbol (intern symbol-name :cl)))
    (cond
      ((fboundp symbol)
       (let ((fn (symbol-function symbol)))
         (list :symbol symbol-name
               :type (typecase fn
                       (standard-method "method")
                       (generic-function "generic-function")
                       (compiled-function "compiled-function")
                       (t "function"))
               :lambda-list (ignore-errors (multiple-value-list (function-lambda-expression fn))))))
      ((macro-function symbol)
       (list :symbol symbol-name :type "macro"))
      (t
       (list :error t :message (format nil "~a is not a function" symbol-name))))))

(defun inspect-package (package-name)
  "Inspect package contents."
  (let ((package (find-package (string-upcase package-name))))
    (unless package
      (return-from inspect-package
        (list :error t
              :message (format nil "Package ~a not found" package-name))))
    (let ((external-count 0)
          (internal-count 0)
          (use-packages nil)
          (used-by nil))
      (do-external-symbols (s package)
        (incf external-count))
      (do-symbols (s package)
        (when (eq (symbol-package s) package)
          (incf internal-count)))
      (setf use-packages (mapcar #'package-name (package-use-list package)))
      (setf used-by (mapcar #'package-name (package-used-by-list package)))
      (list :name (package-name package)
            :nicknames (package-nicknames package)
            :exports-count external-count
            :internal-count internal-count
            :use-packages use-packages
            :used-by used-by))))
