;;;; scripts/run-tests.lisp - run the Rove suite against THIS checkout.
;;;;
;;;; Usage (from anywhere):
;;;;   sbcl --noinform --non-interactive --load scripts/run-tests.lisp
;;;;
;;;; Registers the checkout containing this script at the front of ASDF's registry so a
;;;; same-named copy under ~/quicklisp/local-projects cannot shadow it, then exits with a
;;;; non-zero status when any test fails (so CI and shells can rely on the exit code).
;;;;
;;;; Output: the full Rove output (every pass) goes to reports/test-run.log. The console shows
;;;; only the problems, without colour codes or backtraces, ready to copy/paste. The same text
;;;; is written to reports/test-problems.log (both ignored by version control; set
;;;; TRON_TEST_LOG_DIR to change the directory). Tron's own log4cl lines (deliberate error-path
;;;; tests log at ERROR) still reach the console ahead of the summary; copy test-problems.log.

(require :asdf)

(defvar *root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(push *root* asdf:*central-registry*)
(format t "~&;; Testing checkout: ~a~%" *root*)

(ql:quickload :rove :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :cl-tron-mcp/tests :silent t)
(format t "~&;; cl-tron-mcp resolved to: ~a~%"
        (asdf:system-source-directory :cl-tron-mcp))

(defun log-dir ()
  (let ((dir (uiop:getenv "TRON_TEST_LOG_DIR")))
    (ensure-directories-exist
     (if (and dir (plusp (length dir)))
         (uiop:ensure-directory-pathname dir)
         (merge-pathnames "reports/" *root*)))))

(declaim (ftype function condense-failures))

(defun problem-lines (text)
  "Return the failure section of Rove TEXT with colour codes and backtrace frames removed."
  (let* ((clean (cl-ppcre:regex-replace-all (format nil "~c\\[[0-9;]*m" #\Esc) text ""))
         (lines (uiop:split-string clean :separator '(#\Newline)))
         (start (position-if (lambda (l) (cl-ppcre:scan "tests? failed" l)) lines)))
    (when start
      (condense-failures
       (remove-if (lambda (l) (cl-ppcre:scan "^\\s+\\d+: " l))
                  (nthcdr start lines))))))

(defun truncate-line (line &optional (max 200))
  (if (> (length line) max)
      (concatenate 'string (subseq line 0 max) " ...")
      line))

(defun condense-failures (lines)
  "Keep, per failure, its header, message, and key lines (exit codes, source location).
Long dumps (embedded shell scripts, subprocess objects) stay in the full log only."
  (let ((out '()) (block-lines '()) (kept 0))
    (flet ((flush ()
             (let ((dropped (- (length block-lines) kept)))
               (when (plusp dropped)
                 (push (format nil "   (~d more lines in test-run.log)" dropped) out)))
             (setf block-lines '() kept 0)))
      (dolist (l lines)
        (cond ((cl-ppcre:scan "^\\d+\\) " l)
               (flush) (push "" out) (push l out))
              ((string= (string-trim " " l) ""))
              (t (push l block-lines)
                 (when (or (< kept 5)
                           (cl-ppcre:scan "error code|^\\s+at \\./|^Summary:|tests? failed|^\\s+- " l))
                   (incf kept)
                   (push (truncate-line l) out)))))
      (flush))
    (nreverse out)))

(defun run-suite ()
  "Run the suite; return (values passed-p full-output-text)."
  (let* ((passed nil)
         (text (with-output-to-string (out)
                 (let ((*standard-output* out)
                       (*error-output* out))
                   (setf passed (rove:run :cl-tron-mcp/tests))))))
    (values passed text)))

(multiple-value-bind (passed text) (run-suite)
  (let* ((dir (log-dir))
         (full (merge-pathnames "test-run.log" dir))
         (problems (merge-pathnames "test-problems.log" dir))
         (lines (problem-lines text)))
    (with-open-file (s full :direction :output :if-exists :supersede)
      (write-string text s))
    (with-open-file (s problems :direction :output :if-exists :supersede)
      (format s "~{~a~%~}" lines))
    (cond (passed (format t "~&ALL TESTS PASSED~%"))
          (t (format t "~&~{~a~%~}" lines)
             (format t "~&~%;; problems: ~a~%;; full log: ~a~%" problems full)))
    (uiop:quit (if passed 0 1))))
