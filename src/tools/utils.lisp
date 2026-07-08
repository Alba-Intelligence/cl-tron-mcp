;;;; src/tools/package.lisp
;;;; Define package for all tools

(in-package :cl-tron-mcp/tools)

;; Utility functions for tool handling
(defun get-tool-metadata (name)
  "Get metadata for a specific tool by name."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (list
        :name (tool-entry-name entry)
        :description (get-tool-description (tool-entry-name entry))
        :requires-approval (tool-requires-user-approval-p (tool-entry-name entry))
        :concurrency (or (tool-entry-concurrency (if (tool-entryp entry) entry nil)) "sequential")))))

(defun update-tool-metadata (name metadata-update)
  "Update metadata for an existing tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (loop for (key value) on metadata-update by #'cddr
            do (setf (gethash key (tool-entry-descriptor entry)) value))
      entry)))

(defun normalize-tool-name (name)
  "Normalize tool name to underscore_case format."
  (with-output-to-string (out)
    (loop for i from 0 below (length name)
          for char = (char name i)
          do (cond
               ((char= char #\-)
                (write-char #\_ out))
               ((and (alpha-char-p char) (upper-case-p char))
                (when (or (= i 0) (lower-case-p (char name (1- i))) (digit-char-p (char name (1- i))))
                  (write-char #\_ out))
                (write-char (char-downcase char) out))
               (t
                (write-char (char-downcase char) out))))))

(defun normalize-tool-description (desc)
  "Normalize tool description for consistent display."
  (when desc
    (string-trim #(#\Space #\Newline #\Tab)
                  (let ((result (string-downcase desc)))
                    (loop for i from 0 below (length result)
                          for char = (char result i)
                          when (and (lower-case-p char) (> i 0)
                                    (or (upper-case-p (char result (1- i)))
                                        (digit-char-p (char result (1- i))))
                                    (or (lower-case-p (char result (+ i 1)))))
                            do (setf (char result (+ i 1)) (char-upcase (char result (+ i 1))))
                          finally (return result))))))

(defun get-tool-names-by-category (category)
  "Get list of tool names by category (e.g., 'debugger', 'inspector', 'hot-reload')."
  (let ((names nil))
    (maphash (lambda (name entry)
               (declare (ignore entry))
               (when (and name
                          (search (string-downcase category) (string-downcase (symbol-name name))))
                 (push name names)))
             *tool-registry*)
    (sort names #'string<
          :key (lambda (name) (string-downcase (symbol-name name))))))

(defun get-available-tools ()
  "Get list of all available tool names.
   Returns a list of symbols with normalized names."  (mapcar #'car
          (loop for (name desc) in (list-tool-descriptors)
                collect (cons name desc))))

(defun validate-tool-availability (tool-name)
  "Validate if a tool is available and active."
  (let ((entry (gethash (intern (string-upcase tool-name) :keyword)
                        *tool-registry*)))
    (and entry
         (tool-entry-handler entry)
         (not (gethash :error (tool-entry-descriptor entry))))))