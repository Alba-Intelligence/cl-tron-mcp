;;;; tests/docs-test.lisp - documentation must match the code.
;;;;
;;;; These checks keep docs/ honest: tool names must exist, links must resolve, generated
;;;; files must be fresh, every tool must have a page, and no hand-typed tool counts.
;;;; Planned (not yet implemented) tools belong in docs/plans/, which is not checked.

(in-package :cl-tron-mcp/tests)

(defun doc-root () (asdf:system-source-directory :cl-tron-mcp))
(defun doc-path (relative) (merge-pathnames relative (doc-root)))
(defun doc-text (path) (uiop:read-file-string path :external-format :utf-8))

(defun doc-relative (path)
  (enough-namestring path (doc-root)))

(defun doc-collect (directory pattern &key (skip '()))
  "All files matching PATTERN under DIRECTORY, skipping subdirectory names in SKIP."
  (let ((files (uiop:directory-files directory pattern)))
    (dolist (sub (uiop:subdirectories directory))
      (unless (member (car (last (pathname-directory sub))) skip :test #'string=)
        (setf files (append files (doc-collect sub pattern :skip skip)))))
    files))

(defun doc-markdown-files ()
  "User- and developer-facing markdown (plans, historical specs and reports are exempt)."
  (append
   (loop for name in '("AGENTS.md" "README.md" "CONTRIBUTING.md") ; CHANGELOG.md is history
         for path = (doc-path name)
         when (probe-file path) collect path)
   (doc-collect (doc-path "docs/") "*.md" :skip '("plans" "superpowers"))
   (doc-collect (doc-path "tutorial/") "*.md")
   (doc-collect (doc-path "examples/") "*.md")))

(defun doc-registered-tool-names ()
  (let (names)
    (maphash (lambda (k v) (declare (ignore v)) (push k names))
             cl-tron-mcp/tools::*tool-registry*)
    names))

(defparameter *doc-tool-prefixes*
  '("repl" "swank" "debugger" "inspect" "code" "profile" "sprof" "trace" "thread" "breakpoint"
    "memory" "gc" "health" "metrics" "log" "who" "list" "whitelist" "source" "step" "runtime"
    "system" "reload" "skill"))

(defparameter *doc-not-tools*
  '("system_name" "thread_id" "log_level" "approval_request_id" "request_id" "restart_index"
    "class_name" "symbol_name" "object_id" "max_depth" "slot_name" "target_platform"
    "communication_style" "sbcl_binary" "extra_eval" "function_name" "package_name"
    "log_message" "list_callees_of")
  "Parameter names and similar tokens that look like tool names but are not.")

(defun doc-tool-like-tokens (text)
  (let (tokens)
    (cl-ppcre:do-register-groups (token)
        ("`([a-z][a-z0-9]*(?:_[a-z0-9]+)+)`" text)
      (let ((prefix (subseq token 0 (position #\_ token))))
        (when (and (member prefix *doc-tool-prefixes* :test #'string=)
                   (not (member token *doc-not-tools* :test #'string=)))
          (pushnew token tokens :test #'string=))))
    tokens))

(deftest doc-tool-names-exist-test
  (testing "Every backticked tool name in the docs and prompts is a registered tool"
    (let ((tools (doc-registered-tool-names))
          (offenders '()))
      (dolist (file (append (doc-markdown-files)
                            (doc-collect (doc-path "src/prompts/") "*.lisp")))
        (dolist (token (doc-tool-like-tokens (doc-text file)))
          (unless (member token tools :test #'string=)
            (push (format nil "~a: ~a" (doc-relative file) token) offenders))))
      (ok (null offenders) (format nil "unknown tool names: ~{~%  ~a~}" (sort offenders #'string<))))))

(deftest doc-links-resolve-test
  (testing "Relative markdown links point at existing files"
    (let ((broken '()))
      (dolist (file (doc-markdown-files))
        (cl-ppcre:do-register-groups (target)
            ("\\]\\(([^)\\s#]+)(?:#[^)]*)?\\)" (doc-text file))
          (unless (or (cl-ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*:" target) ; http:, mailto:, ...
                      (char= (char target 0) #\/))
            (let ((resolved (merge-pathnames target (uiop:pathname-directory-pathname file))))
              (unless (ignore-errors (probe-file resolved))
                (push (format nil "~a -> ~a" (doc-relative file) target) broken))))))
      (ok (null broken) (format nil "broken links: ~{~%  ~a~}" (sort broken #'string<))))))

(deftest doc-every-tool-has-a-page-test
  (testing "Every registered tool has docs/tools/<name>.md"
    (let ((missing (loop for name in (doc-registered-tool-names)
                         unless (probe-file (doc-path (format nil "docs/tools/~a.md"
                                                              (substitute #\- #\_ name))))
                           collect name)))
      (ok (null missing) (format nil "tools without a page: ~{~a ~}" (sort missing #'string<))))))

(deftest doc-resource-whitelist-exists-test
  (testing "Every MCP resource in the whitelist exists on disk"
    (let ((missing (loop for rel in cl-tron-mcp/resources::*resource-whitelist*
                         unless (probe-file (doc-path rel)) collect rel)))
      (ok (null missing) (format nil "missing resources: ~{~a ~}" missing)))))

(deftest doc-generated-files-fresh-test
  (testing "docs/using/tools.md and errors.md match what scripts/gen-docs.lisp generates"
    (unless (find-package :cl-tron-mcp/docgen)
      (setf (symbol-value (intern "*GEN-DOCS-NO-RUN*" :cl-user)) t)
      (load (doc-path "scripts/gen-docs.lisp")))
    (flet ((gen (name) (funcall (intern name :cl-tron-mcp/docgen))))
      (ok (string= (gen "GENERATE-TOOLS-MD") (doc-text (doc-path "docs/using/tools.md")))
          "docs/using/tools.md is stale: run scripts/gen-docs.lisp")
      (ok (string= (gen "GENERATE-ERRORS-MD") (doc-text (doc-path "docs/using/errors.md")))
          "docs/using/errors.md is stale: run scripts/gen-docs.lisp"))))

(deftest doc-no-hand-typed-tool-counts-test
  (testing "Tool counts appear only in the generated catalogue"
    (let ((offenders '()))
      (dolist (file (doc-markdown-files))
        (unless (string= (doc-relative file) "docs/using/tools.md")
          (when (cl-ppcre:scan "\\b[0-9]+ (registered )?(tools|categories)\\b" (doc-text file))
            (push (doc-relative file) offenders))))
      (ok (null offenders)
          (format nil "remove hand-typed counts from: ~{~%  ~a~}" (sort offenders #'string<))))))
