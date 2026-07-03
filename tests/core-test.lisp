;;;; tests/core-test.lisp

(in-package :cl-tron-mcp/tests)

(deftest version-test
    (testing "Version is defined"
             (ok (stringp cl-tron-mcp/core:*version*))))

(deftest config-test
    (testing "Config can be set and retrieved"
             (cl-tron-mcp/core:set-config :test-key "test-value")
             (ok (string= (cl-tron-mcp/core:get-config :test-key) "test-value"))))

(deftest tracer-test
    (testing "Tracer can add and remove trace"
             (let ((result (cl-tron-mcp/tracer:trace-function "cl-tron-mcp/core::dummy-func")))
               (ok (getf result :success)))
             (let ((result (cl-tron-mcp/tracer:trace-list)))
               (ok (getf result :count)))
             (let ((result (cl-tron-mcp/tracer:trace-remove "cl-tron-mcp/core::dummy-func")))
               (ok (getf result :success)))))

(deftest create-configs-prefers-run-mcp-when-devenv-is-needed-test
    (testing "Config generation uses run-mcp.sh when devenv is available but no host Lisp is on PATH"
             (let* ((repo-root (asdf:system-source-directory :cl-tron-mcp))
                    (command (format nil
                                     "set -euo pipefail~%tmp=$(mktemp -d)~%
trap 'rm -rf \"$tmp\"' EXIT~%
home_dir=\"$tmp/home\"~%
bin_dir=\"$tmp/bin\"~%
mkdir -p \"$home_dir/.copilot\" \"$bin_dir\"~%
for tool in bash basename cat dirname mkdir devenv; do~%
  ln -sf \"$(command -v \"$tool\")\" \"$bin_dir/$tool\"~%
done~%
HOME=\"$home_dir\" PATH=\"$bin_dir\" ~a --client copilot-cli >/dev/null~%
cat \"$home_dir/.copilot/mcp-config.json\""
                                     (namestring (merge-pathnames "create_configs.sh" repo-root))))
                    (config (uiop:run-program (list "bash" "-lc" command)
                                              :output :string)))
               (ok (search "./run-mcp.sh --stdio-only" config)))))
