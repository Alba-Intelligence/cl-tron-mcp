;;;; tests/installation-test.lisp - regression tests for the installation/skill tools (E1)

(in-package :cl-tron-mcp/tests)

(deftest skill-store-installation-guide-tool-test
  (testing "skill_store_installation_guide returns a guide instead of an error"
    (let ((result (cl-tron-mcp/tools::call-tool
                   "skill_store_installation_guide"
                   (list :|target_platform| "claude" :|format| "json"))))
      (ok (listp result))
      (ok (not (getf result :error)))
      (ok (stringp (getf result :documentation)))
      (ok (search "Claude" (getf result :documentation))))))
