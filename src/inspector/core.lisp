;;;; src/inspector/core.lisp

(in-package :cl-tron-mcp/inspector)

(defun inspect-object (object-id &key (max-depth 3) (max-elements 100))
  "Inspect object by ID."
  (let ((object (cl-tron-mcp/sbcl:lookup-object object-id)))
    (unless object
      (return-from inspect-object
        (cl-tron-mcp/core:make-error "OBJECT_NOT_FOUND"
                                     :details (list :object-id object-id))))
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

(defun inspect-slot (&key object_id slot_name value)
  "Get or set slot value."
  (let ((object (cl-tron-mcp/sbcl:lookup-object object_id)))
    (unless object
      (return-from inspect-slot
        (cl-tron-mcp/core:make-error "OBJECT_NOT_FOUND"
                                     :details (list :object-id object_id))))
    (let ((slot-symbol (intern (string-upcase slot_name) :keyword)))
      (when value
        (setf (slot-value object slot-symbol) value))
      (list :slot slot_name
            :value (format nil "~a"
                          (ignore-errors
                            (slot-value object slot-symbol)))))))

(defun inspect-class (&key class_name)
  "Inspect class definition using CLOS MOP."
  (let ((class (find-class (intern (string-upcase class_name) :cl) nil)))
    (unless class
      (return-from inspect-class
        (cl-tron-mcp/core:make-error "CLASS_NOT_FOUND"
                                     :details (list :class-name class_name))))
    (list :name (class-name class)
          :direct-superclasses (mapcar #'class-name (closer-mop:class-direct-superclasses class))
          :direct-slots (mapcar (lambda (s)
                                  (list :name (closer-mop:slot-definition-name s)))
                                (closer-mop:class-direct-slots class))
          :precedence-list (mapcar #'class-name (closer-mop:class-precedence-list class)))))

(defun inspect-function (&key symbol_name)
  "Inspect function definition."
  (let* ((name (string symbol_name))
         (package-str (if (find #\: name) (subseq name 0 (position #\: name)) "CL"))
         (symbol-name-only (if (find #\: name) (subseq name (1+ (position #\: name))) name))
         (package (or (find-package (string-upcase package-str)) (find-package :cl)))
         (symbol (find-symbol (string-upcase symbol-name-only) package)))
    (cond
      ((and symbol (fboundp symbol))
       (let ((fn (symbol-function symbol)))
         (list :symbol (format nil "~a::~a" package-str (string-upcase symbol-name-only))
               :type (typecase fn
                       (standard-method "method")
                       (generic-function "generic-function")
                       (compiled-function "compiled-function")
                       (t "function"))
               :lambda-list (ignore-errors (multiple-value-list (function-lambda-expression fn))))))
      ((and symbol (macro-function symbol))
       (list :symbol (format nil "~a::~a" package-str (string-upcase symbol-name-only)) :type "macro"))
(t
        (cl-tron-mcp/core:make-error "NOT_A_FUNCTION"
                                     :details (list :symbol symbol-name))))))

(defun inspect-package (&key package_name)
  "Inspect package contents."
  (let ((package (find-package (string-upcase package_name))))
    (unless package
      (return-from inspect-package
        (cl-tron-mcp/core:make-error "PACKAGE_NOT_FOUND"
                                     :details (list :package-name package_name))))
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
