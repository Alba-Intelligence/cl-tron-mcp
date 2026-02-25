;;;; src/resources/handler.lisp
;;;;
;;;; Implementation of MCP Resources for cl-tron-mcp.
;;;;
;;;; This file provides:
;;;;   - Resource listing (resources/list)
;;;;   - Resource reading (resources/read)
;;;;   - Security controls (whitelist, path traversal prevention)
;;;;
;;;; Resources are documentation files that AI agents can discover and read
;;;; to understand how to use the MCP server. This addresses the discoverability
;;;; problem where agents didn't know about swank_connect and other tools.
;;;;
;;;; Usage:
;;;;   MCP clients call resources/list to get available docs
;;;;   MCP clients call resources/read with a URI to get contents
;;;;
;;;; Security Model:
;;;;   1. Whitelist: Only explicitly listed files are exposed
;;;;   2. Path Validation: All paths are resolved and checked against project root
;;;;   3. No Secrets: Files like .env, secrets/, credentials are never exposed

(in-package :cl-tron-mcp/resources)

;;; ============================================================
;;; Condition Types
;;; ============================================================

(define-condition resource-not-found-error (error)
  ((uri :initarg :uri :reader resource-not-found-uri)
   (message :initarg :message :reader resource-not-found-message))
  (:report (lambda (condition stream)
             (format stream "Resource not found: ~a (~a)"
                     (resource-not-found-uri condition)
                     (resource-not-found-message condition)))))

(define-condition resource-access-denied-error (error)
  ((uri :initarg :uri :reader resource-access-denied-uri)
   (reason :initarg :reason :reader resource-access-denied-reason))
  (:report (lambda (condition stream)
             (format stream "Access denied: ~a (~a)"
                     (resource-access-denied-uri condition)
                     (resource-access-denied-reason condition)))))

;;; ============================================================
;;; Resource Whitelist
;;; ============================================================
;;;
;;; Only files in this whitelist are exposed via MCP Resources.
;;; This is a security measure to prevent accidental exposure of
;;; sensitive files like .env, credentials, etc.
;;;
;;; Each entry is a relative path from the project root.

(defvar *resource-whitelist* nil
  "List of files that can be exposed as MCP resources.")

(defun initialize-default-whitelist ()
  "Initialize the whitelist with default documentation files.
This is called when the module is loaded."
  (setf *resource-whitelist*
        (list
         ;; Core documentation
         "AGENTS.md"
         "README.md"
         
         ;; Tool documentation
         "docs/tools/debugger.md"
         "docs/tools/inspector.md"
         "docs/tools/hot-reload.md"
         "docs/tools/profiler.md"
         "docs/tools/threads.md"
         "docs/tools/monitor.md"
         
         ;; Architecture and integration
         "docs/architecture.md"
         "docs/starting-the-mcp.md"
         "docs/swank-integration.md"
         "tutorial/e2e-mcp-workflow.md")))

;; Initialize on load
(initialize-default-whitelist)

(defun add-resource-to-whitelist (relative-path)
  "Add RELATIVE-PATH to the resource whitelist.
Path should be relative to project root."
  (pushnew relative-path *resource-whitelist* :test #'string=))

(defun clear-resource-whitelist ()
  "Clear all entries from the whitelist.
Use with caution - this will hide all resources from MCP clients."
  (setf *resource-whitelist* nil))

;;; ============================================================
;;; Path Resolution and Security
;;; ============================================================

(defun get-project-root ()
  "Get the project root directory.
Returns the directory containing cl-tron-mcp.asd."
  (let ((asd-path (asdf:system-source-file (asdf:find-system :cl-tron-mcp))))
    (if asd-path
        (make-pathname :directory (pathname-directory asd-path))
        (uiop:pathname-directory-pathname (truename *default-pathname-defaults*)))))

(defun resolve-resource-path (uri)
  "Resolve URI to an absolute pathname.
Returns NIL if the path is invalid or outside project root.
URI should be in format: file://relative/path/to/file.md"
  (let ((project-root (get-project-root)))
    (cond
      ;; Handle file:// URIs
      ((string-prefix-p "file://" uri)
       (let* ((relative-path (subseq uri 7))
              (full-path (merge-pathnames relative-path project-root)))
         ;; Security check: ensure resolved path is within project root
         (let ((resolved (probe-file full-path)))
           (when (and resolved
                      (path-in-directory-p resolved project-root))
             resolved))))
      
      ;; Handle bare relative paths (for convenience)
      ((not (find #\: uri))
       (let ((full-path (merge-pathnames uri project-root)))
         (let ((resolved (probe-file full-path)))
           (when (and resolved
                      (path-in-directory-p resolved project-root))
             resolved))))
      
      (t nil))))

(defun path-in-directory-p (path directory)
  "Check if PATH is within DIRECTORY.
This prevents path traversal attacks."
  (let ((path-str (namestring (truename path)))
        (dir-str (namestring (truename directory))))
    (string-prefix-p dir-str path-str)))

(defun string-prefix-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

;;; ============================================================
;;; Resource Descriptors
;;; ============================================================

(defstruct resource-descriptor
  "Structure representing an MCP resource."
  uri
  name
  title
  description
  mime-type
  size)

(defun make-resource-uri (relative-path)
  "Create a resource URI from a relative path.
Format: file://relative/path/to/file.md"
  (format nil "file://~a" relative-path))

(defun get-resource-name (relative-path)
  "Extract the filename from a relative path."
  (file-namestring (pathname relative-path)))

(defun guess-mime-type (pathname)
  "Guess MIME type from file extension."
  (let ((extension (string-downcase (pathname-type pathname))))
    (case (intern extension :keyword)
      (:md "text/markdown")
      (:txt "text/plain")
      (:json "application/json")
      (:lisp "text/x-common-lisp")
      (:asd "text/x-common-lisp")
      (t "application/octet-stream"))))

(defun file-size (pathname)
  "Get file size in bytes."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (file-length stream)))

;;; ============================================================
;;; Resource Listing (resources/list)
;;; ============================================================

(defun list-resources ()
  "Return a list of resource descriptors for all whitelisted files.
This implements the MCP resources/list operation."
  (let ((project-root (get-project-root)))
    (loop for relative-path in *resource-whitelist*
          for full-path = (merge-pathnames relative-path project-root)
          for probed = (probe-file full-path)
          when probed
            collect (make-resource-descriptor
                     :uri (make-resource-uri relative-path)
                     :name (get-resource-name relative-path)
                     :title (format nil "Documentation: ~a" (get-resource-name relative-path))
                     :description (format nil "MCP documentation file: ~a" relative-path)
                     :mime-type (guess-mime-type probed)
                     :size (file-size probed)))))

(defun handle-resources-list (id)
  "Handle MCP resources/list request.
Returns plist suitable for JSON serialization."
  (let ((resources (list-resources)))
    (list :|jsonrpc| "2.0"
          :|id| id
          :|result|
          (list :|resources|
                (loop for res in resources
                      collect (list :|uri| (resource-descriptor-uri res)
                                   :|name| (resource-descriptor-name res)
                                   :|title| (resource-descriptor-title res)
                                   :|description| (resource-descriptor-description res)
                                   :|mimeType| (resource-descriptor-mime-type res)
                                   :|size| (resource-descriptor-size res)))))))

;;; ============================================================
;;; Resource Reading (resources/read)
;;; ============================================================

(defun read-resource (uri)
  "Read the contents of a resource by URI.
Returns the file contents as a string.
Signals RESOURCE-NOT-FOUND-ERROR if the resource doesn't exist.
Signals RESOURCE-ACCESS-DENIED-ERROR if the resource is not whitelisted."
  (let* ((relative-path (if (string-prefix-p "file://" uri)
                            (subseq uri 7)
                            uri))
         (whitelisted-p (member relative-path *resource-whitelist* :test #'string=)))
    (unless whitelisted-p
      (error 'resource-access-denied-error
             :uri uri
             :reason "Resource not in whitelist"))
    
    (let ((resolved-path (resolve-resource-path uri)))
      (unless resolved-path
        (error 'resource-not-found-error
               :uri uri
               :message "File not found or outside project directory"))
      
      (with-open-file (stream resolved-path :direction :input :external-format :utf-8)
        (let ((contents (make-string (file-length stream))))
          (read-sequence contents stream)
          (values contents
                  (guess-mime-type resolved-path)
                  (file-size resolved-path)))))))

(defun handle-resources-read (id params)
  "Handle MCP resources/read request.
PARAMS should contain :|uri| with the resource URI.
Returns plist suitable for JSON serialization."
  (let ((uri (getf params :|uri|)))
    (handler-case
        (multiple-value-bind (contents mime-type size)
            (read-resource uri)
          (list :|jsonrpc| "2.0"
                :|id| id
                :|result|
                (list :|contents|
                      (list (list :|uri| uri
                                 :|mimeType| mime-type
                                 :|text| contents
                                 :|size| size)))))
      (resource-not-found-error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32002
                   :|message| (format nil "Resource not found: ~a"
                                      (resource-not-found-uri e)))))
      (resource-access-denied-error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32003
                   :|message| (format nil "Access denied: ~a"
                                      (resource-access-denied-reason e)))))
      (error (e)
        (list :|jsonrpc| "2.0"
              :|id| id
              :|error|
              (list :|code| -32603
                   :|message| (princ-to-string e)))))))

;;; ============================================================
;;; Module Documentation
;;; ============================================================

(defun resources-help ()
  "Return help text for the resources module."
  (list :module "cl-tron-mcp/resources"
        :description "MCP Resources implementation for exposing documentation"
        :operations (list
                    (list :name "resources/list"
                          :description "List all available documentation resources")
                    (list :name "resources/read"
                          :description "Read contents of a specific resource by URI"))
        :whitelisted-files *resource-whitelist*))

(provide :cl-tron-mcp/resources)
