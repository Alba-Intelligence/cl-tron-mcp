;;;; scripts/run-tests.lisp - run the Rove suite against THIS checkout.
;;;;
;;;; Usage (from anywhere):
;;;;   sbcl --noinform --non-interactive --load scripts/run-tests.lisp
;;;;
;;;; Registers the checkout containing this script at the front of ASDF's registry so a
;;;; same-named copy under ~/quicklisp/local-projects cannot shadow it, then exits with a
;;;; non-zero status when any test fails (so CI and shells can rely on the exit code).

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (push root asdf:*central-registry*)
  (format t "~&;; Testing checkout: ~a~%" root))

(ql:quickload :rove :silent t)
(ql:quickload :cl-tron-mcp/tests :silent t)
(format t "~&;; cl-tron-mcp resolved to: ~a~%"
        (asdf:system-source-directory :cl-tron-mcp))

(uiop:quit (if (rove:run :cl-tron-mcp/tests) 0 1))
