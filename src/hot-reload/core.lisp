;;;; src/hot-reload/core.lisp

(in-package :cl-tron-mcp/hot-reload)

(defun compile-and-load (code &key filename position package)
  "Compile and load a Lisp code string into the current image.
For top-level forms (defun, defvar, etc.), uses eval which SBCL compiles.
This is the local fallback; when Swank is connected, use repl-compile instead."
  (declare (ignore position))
  (handler-case
      (let ((*package* (or (and package (find-package package))
                           (find-package :cl-user))))
        (unless *package*
          (error "Package ~a not found" package))
        (with-input-from-string (stream code)
          (loop with *read-eval* = nil
                for form = (read stream nil :eof)
                until (eq form :eof)
                do (eval form)))
        (list :success t
              :message (format nil "~a compiled and loaded"
                               (or filename "code"))))
    (error (e)
      (list :error t
            :type "COMPILATION-ERROR"
            :message (princ-to-string e)))))

(defun reload-package (package-name &key recursive)
  "Reload all source files in a package."
  (handler-case
      (let ((package (find-package (string-upcase package-name))))
        (unless package
          (return-from reload-package
            (list :error t
                  :message (format nil "Package ~a not found" package-name))))
        (let ((files (get-package-source-files package)))
          ;; Never report success for a reload that did nothing.
          (unless files
            (return-from reload-package
              (list :error t
                    :message (format nil "Cannot determine source files for package ~a; reload the owning ASDF system with reload_system instead"
                                     package-name))))
          (dolist (file files)
            (load (compile-file file)))
          (list :success t
                :files-reloaded (length files))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun reload-system (system-name &key force)
  "Reload ASDF system with dependencies."
  (handler-case
      (progn
        (asdf:load-system (string-downcase system-name)
                          :force (if force :all force))
        (list :success t
              :system system-name))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun replace-function (function-name new-code)
  "Atomically replace function definition."
  (handler-case
      ;; *READ-EVAL* is off: agent-supplied text must not run code at read time.
      (let* ((*read-eval* nil)
             (symbol (read-from-string function-name)))
        (unless (and symbol (symbolp symbol) (fboundp symbol))
          (return-from replace-function
            (list :error t
                  :message (format nil "Function ~a not found" function-name))))
        (let ((old-def (symbol-function symbol))
              (replaced nil))
          ;; Roll back ONLY if the new code fails. (The cleanup form of an unwind-protect
          ;; also runs on normal exit, which previously undid every successful replacement.)
          (unwind-protect
               (progn
                 (with-input-from-string (stream new-code)
                   (loop for form = (read stream nil :eof)
                         until (eq form :eof)
                         do (eval form)))
                 (setf replaced t)
                 (list :success t
                       :function function-name))
            (unless replaced
              (setf (symbol-function symbol) old-def)))))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))

(defun get-source-location (symbol-name &key (type :function))
  "Return the source locations of SYMBOL-NAME's definition(s) of TYPE (:function, :variable,
:class, :macro ...) as (:symbol name :type type :locations ((:file path :form-path path) ...)).
SYMBOL-NAME may be package-qualified. Returns an error plist when the symbol, or any source
information for it, is not found, or when this implementation cannot supply source locations
(only SBCL does, via sb-introspect)."
  #+sbcl
  (let ((symbol (handler-case (let ((*read-eval* nil)) (read-from-string symbol-name))
                  (error () nil))))
    (cond
      ((not (symbolp symbol))
       (list :error t :message (format nil "~a is not a symbol name" symbol-name)))
      ((not (or (fboundp symbol) (boundp symbol) (find-class symbol nil)))
       (list :error t :message (format nil "Symbol ~a not found or has no definitions" symbol-name)))
      (t
       (let ((sources (ignore-errors (sb-introspect:find-definition-sources-by-name symbol type))))
         (if (null sources)
             (list :error t
                   :message (format nil "No ~(~a~) definition source found for ~a" type symbol-name))
             (list :symbol symbol-name
                   :type type
                   :locations (loop for src in sources
                                    for file = (sb-introspect:definition-source-pathname src)
                                    collect (list :file (and file (namestring file))
                                                  :form-path (sb-introspect:definition-source-form-path src)))))))))
  #-sbcl
  (progn symbol-name type
         (list :error t :message "Source locations are only available on SBCL")))

(defun get-package-source-files (package)
  "Get list of source files for package. Placeholder implementation."
  (declare (ignore package))
  nil)
