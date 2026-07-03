;;;; tests/hot-reload-test.lisp - Unit tests for hot-reload tools

(in-package :cl-tron-mcp/tests)

(deftest compile-and-load-test
    (testing "compile-and-load compiles a simple defun"
             (let ((result (cl-tron-mcp/hot-reload:compile-and-load
                            "(defun cl-tron-mcp/tests::__hot-reload-test-fn () 42)")))
               (ok (listp result))
               (ok (or (getf result :success) (getf result :compiled)))))
  (testing "compile-and-load evaluates all top-level forms"
           (let* ((package-name "CL-TRON-MCP/HOT-RELOAD-TEST-MULTI")
                 (function-name "MULTI-FORM-TEST")
                 (code (format nil "(defpackage ~a (:use :cl))~%(in-package ~a)~%(defun ~a () :ok)"
                               package-name package-name function-name)))
            (when (find-package package-name)
              (delete-package package-name))
            (unwind-protect
                 (let ((result (cl-tron-mcp/hot-reload:compile-and-load code)))
                   (ok (getf result :success))
                   (let* ((package (find-package package-name))
                          (symbol (and package
                                       (find-symbol function-name package))))
                     (ok package)
                     (ok symbol)
                     (ok (fboundp symbol))
                     (ok (eq :ok (funcall symbol)))))
              (when (find-package package-name)
                (delete-package package-name)))))
  (testing "compile-and-load honors requested package"
           (let* ((package-name "CL-TRON-MCP/HOT-RELOAD-TEST-PACKAGE")
                 (function-name "PACKAGE-SCOPED-FN"))
            (when (find-package package-name)
              (delete-package package-name))
            (make-package package-name :use '(:cl))
            (unwind-protect
                 (let ((result (cl-tron-mcp/hot-reload:compile-and-load
                                (format nil "(defun ~a () :package-ok)" function-name)
                                :package package-name)))
                   (ok (getf result :success))
                   (let* ((package (find-package package-name))
                          (symbol (and package
                                       (find-symbol function-name package))))
                     (ok symbol)
                     (ok (fboundp symbol))
                     (ok (eq :package-ok (funcall symbol)))))
              (when (find-package package-name)
                (delete-package package-name)))))
  (testing "compile-and-load returns error for invalid code"
           (let ((result (cl-tron-mcp/hot-reload:compile-and-load
                          "(defun")))
             (ok (listp result))
             (ok (getf result :error)))))

(deftest reload-system-test
    (testing "reload-system returns error for unknown system"
             (let ((result (cl-tron-mcp/hot-reload:reload-system "no-such-system-xyz-999")))
               (ok (listp result))
               (ok (getf result :error)))))

(deftest get-source-location-test
    (testing "get-source-location returns info for defined function"
             (let ((result (cl-tron-mcp/hot-reload:get-source-location "cl-tron-mcp/core:start-server")))
               (ok (listp result)))))
