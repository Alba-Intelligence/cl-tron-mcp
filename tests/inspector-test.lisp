;;;; tests/inspector-test.lisp - Unit tests for inspector tools

(in-package :cl-tron-mcp/tests)

(deftest inspect-class-test
  (testing "inspect-class returns class info for known class"
    (let ((result (cl-tron-mcp/inspector:inspect-class :class_name "standard-object")))
      (ok (listp result))))
  (testing "inspect-class returns error for unknown class"
    (let ((result (cl-tron-mcp/inspector:inspect-class :class_name "no-such-class-xyz")))
      (ok (listp result)))))

(deftest inspect-function-test
  (testing "inspect-function returns info for known function"
    (let ((result (cl-tron-mcp/inspector:inspect-function :symbol_name "cl:car")))
      (ok (listp result))))
  (testing "inspect-function handles unknown symbol gracefully"
    (let ((result (cl-tron-mcp/inspector:inspect-function :symbol_name "no-such-func-xyz")))
      (ok (listp result)))))

(deftest inspect-package-test
  (testing "inspect-package returns info for CL package"
    (let ((result (cl-tron-mcp/inspector:inspect-package :package_name "CL")))
      (ok (listp result))))
  (testing "inspect-package returns error for unknown package"
    (let ((result (cl-tron-mcp/inspector:inspect-package :package_name "NO-SUCH-PKG-XYZ")))
      (ok (listp result))
      (ok (getf result :error)))))

(deftest inspect-object-test
  (testing "inspect-object with valid registered id returns data"
    ;; Register a simple object first
    (let* ((obj (list 1 2 3))
           (id (cl-tron-mcp/sbcl:register-object obj))
           (result (cl-tron-mcp/inspector:inspect-object id)))
      (ok (listp result))))
  (testing "inspect-object with invalid id returns error"
    (let ((result (cl-tron-mcp/inspector:inspect-object 999999)))
      (ok (listp result))
      (ok (getf result :error)))))
