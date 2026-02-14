;;;; src/tools/registry.lisp

(in-package :cl-tron-mcp/tools)

(defvar *tool-registry* (make-hash-table :test 'equal))

(defstruct tool-entry
  name
  descriptor
  handler)

(defun register-tool (name description &key input-schema output-schema requires-approval concurrency)
  "Register a tool with descriptor and handler."
  (let ((descriptor (make-hash-table :test 'equal)))
    (setf (gethash :name descriptor) name)
    (setf (gethash :description descriptor) description)
    (setf (gethash :inputSchema descriptor) (or input-schema (make-hash-table :test 'equal)))
    (setf (gethash :outputSchema descriptor) (or output-schema (make-hash-table :test 'equal)))
    (setf (gethash :requiresApproval descriptor) (or requires-approval nil))
    (setf (gethash :concurrency descriptor) (or concurrency "sequential"))
    (setf (gethash name *tool-registry*)
          (make-tool-entry :name name
                          :descriptor descriptor
                          :handler nil))))

(defun register-tool-handler (name handler)
  "Register handler function for already registered tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (setf (tool-entry-handler entry) handler))))

(defun list-tool-descriptors ()
  "Get list of all tool descriptors."
  (let ((descriptors (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (name entry)
               (declare (ignore name))
               (vector-push-extend (tool-entry-descriptor entry) descriptors))
             *tool-registry*)
    (coerce descriptors 'list)))

(defun get-tool-handler (name)
  "Get handler function for tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-handler entry))))

(defun call-tool (name arguments)
  "Call tool by name with arguments plist.
The plist keys are JSON-style (e.g., :|port|) and converted to proper keywords (:PORT)."
  (let ((handler (get-tool-handler name)))
    (unless handler
      (error "Unknown tool: ~a" name))
    (let ((args-list (loop for (key value) on arguments by #'cddr
                           for keyword = (intern (string-upcase (string key)) :keyword)
                           append (list keyword value))))
      (apply handler args-list))))
