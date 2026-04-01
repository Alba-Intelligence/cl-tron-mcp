;;;; tests/hot-reload-test.lisp - Unit tests for hot-reload tools

(in-package :cl-tron-mcp/tests)

(deftest compile-and-load-test
    (testing "compile-and-load compiles a simple defun"
             (let ((result (cl-tron-mcp/hot-reload:compile-and-load
                            "(defun cl-tron-mcp/tests::__hot-reload-test-fn () 42)")))
               (ok (listp result))
               (ok (or (getf result :success) (getf result :compiled)))))
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
