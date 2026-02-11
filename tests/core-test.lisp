;;;; tests/core-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest version-test
  (testing "Version is defined"
    (ok (stringp cl-tron-mcp/core:*version*))))

(deftest config-test
  (testing "Config can be set and retrieved"
    (cl-tron-mcp/core:set-config :test-key "test-value")
    (ok (string= (cl-tron-mcp/core:get-config :test-key) "test-value"))))
