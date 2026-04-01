;;;; tests/xref-test.lisp - Unit tests for cross-reference tools

(in-package :cl-tron-mcp/tests)

(deftest who-calls-test
    (testing "who-calls returns callers list for known function"
             (let ((result (cl-tron-mcp/xref:who-calls "cl-tron-mcp/core:start-server")))
               (ok (listp result))))
  (testing "who-calls handles unknown symbol gracefully"
           (let ((result (cl-tron-mcp/xref:who-calls "no-such-function-xyz-999")))
             (ok (listp result)))))

(deftest who-references-test
    (testing "who-references returns results for known variable"
             (let ((result (cl-tron-mcp/xref:who-references "cl-tron-mcp/core:*version*")))
               (ok (listp result)))))

(deftest list-callees-test
    (testing "list-callees returns callees for known function"
             (let ((result (cl-tron-mcp/xref:list-callees "cl-tron-mcp/core:start-server")))
               (ok (listp result)))))

(deftest who-binds-test
    (testing "who-binds returns results for known variable"
             (let ((result (cl-tron-mcp/xref:who-binds "cl-tron-mcp/core:*version*")))
               (ok (listp result)))))

(deftest who-sets-test
    (testing "who-sets returns results for known variable"
             (let ((result (cl-tron-mcp/xref:who-sets "cl-tron-mcp/core:*version*")))
               (ok (listp result)))))
