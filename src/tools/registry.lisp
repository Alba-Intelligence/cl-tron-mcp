;;;; src/tools/registry.lisp

(in-package :cl-tron-mcp/tools)

(defvar *tool-registry* (make-hash-table :test 'equal))

(defstruct tool-entry
  (name "" :type string)
  (descriptor nil :type hash-table)
  (handler nil :type function))

(defun register-tool (name descriptor handler)
  "Register a tool with descriptor and handler."
  (setf (gethash name *tool-registry*)
        (make-tool-entry :name name
                        :descriptor descriptor
                        :handler handler)))

(defun list-tool-descriptors ()
  "Get list of all tool descriptors."
  (let ((descriptors (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (name entry)
               (vector-push-extend (tool-entry-descriptor entry) descriptors))
             *tool-registry*)
    (coerce descriptors 'list)))

(defun get-tool-handler (name)
  "Get handler function for tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-handler entry))))

(defun call-tool (name arguments)
  "Call tool by name with arguments."
  (let ((handler (get-tool-handler name)))
    (unless handler
      (error "Unknown tool: ~a" name))
    (funcall handler arguments)))
