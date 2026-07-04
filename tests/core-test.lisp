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

(deftest ecl-combined-startup-stays-running-test
    (testing "Default ECL startup stays alive and serves HTTP health checks"
             (let ((ecl-path (ignore-errors
                               (string-right-trim
                                '(#\Newline #\Return)
                                (uiop:run-program '("bash" "-lc" "command -v ecl")
                                                  :output :string)))))
               (if (or (null ecl-path) (string= ecl-path ""))
                   (ok t "ECL not installed - skipping")
                   (let* ((repo-root (namestring (asdf:system-source-directory :cl-tron-mcp)))
                          (port 4017)
                          (command (format nil
                                           "set -euo pipefail~%
cd ~a~%
./start-mcp.sh --use-ecl --port ~d >/tmp/tron-ecl-combined.out 2>/tmp/tron-ecl-combined.err &~%
pid=$!~%
cleanup() { kill \"$pid\" 2>/dev/null || true; wait \"$pid\" 2>/dev/null || true; }~%
trap cleanup EXIT~%
for _ in $(seq 1 20); do~%
  if ! kill -0 \"$pid\" 2>/dev/null; then~%
    echo EXITED~%
    exit 0~%
  fi~%
  body=$(curl -s -m 2 http://127.0.0.1:~d/health || true)~%
  if [ \"$body\" = '{\"status\":\"ok\"}' ]; then~%
    printf 'ALIVE|%s' \"$body\"~%
    exit 0~%
  fi~%
  sleep 1~%
done~%
echo TIMEOUT"
                                           repo-root
                                           port
                                           port))
                          (output (uiop:run-program (list "bash" "-lc" command)
                                                    :output :string)))
                     (ok (search "ALIVE|{\"status\":\"ok\"}" output)))))))

(deftest ecl-combined-sigint-shuts-down-cleanly-test
    (testing "Ctrl-C on the long-running ECL launcher shuts down without entering the ECL debugger"
             (let ((ecl-path (ignore-errors
                               (string-right-trim
                                '(#\Newline #\Return)
                                (uiop:run-program '("bash" "-lc" "command -v ecl")
                                                  :output :string)))))
               (if (or (null ecl-path) (string= ecl-path ""))
                   (ok t "ECL not installed - skipping")
                   (let* ((repo-root (namestring (asdf:system-source-directory :cl-tron-mcp)))
                          (port 4018)
                          (command (format nil
                                           "python - <<'PY'~%
import os, signal, subprocess, time, urllib.request~%
repo = ~s~%
port = ~d~%
pid_file = os.path.join(repo, '.tron-server.pid')~%
proc = subprocess.Popen(['./start-mcp.sh', '--use-ecl', '--port', str(port)], cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)~%
ready = False~%
for _ in range(30):~%
    if proc.poll() is not None:~%
        break~%
    try:~%
        with urllib.request.urlopen(f'http://127.0.0.1:{port}/health', timeout=1) as response:~%
            if response.read().decode() == '{\"status\":\"ok\"}':~%
                ready = True~%
                break~%
    except Exception:~%
        pass~%
    time.sleep(1)~%
proc.send_signal(signal.SIGINT)~%
try:~%
    stdout, stderr = proc.communicate(timeout=15)~%
    exited = True~%
except subprocess.TimeoutExpired:~%
    proc.kill()~%
    stdout, stderr = proc.communicate()~%
    exited = False~%
bad_markers = ['INTERACTIVE-INTERRUPT', 'UNBOUND-VARIABLE', 'Excessive debugger depth']~%
clean = ready and exited and all(marker not in stdout and marker not in stderr for marker in bad_markers) and not os.path.exists(pid_file)~%
print(f'READY={int(ready)}')~%
print(f'EXITED={int(exited)}')~%
print(f'PID_FILE_PRESENT={int(os.path.exists(pid_file))}')~%
print(f'CLEAN={int(clean)}')~%
print('---STDOUT---')~%
print(stdout)~%
print('---STDERR---')~%
print(stderr)~%
PY"
                                           repo-root
                                           port))
                          (output (uiop:run-program (list "bash" "-lc" command)
                                                    :output :string)))
                     (ok (search "CLEAN=1" output) output))))))

(deftest ecl-combined-stop-command-succeeds-test
    (testing "--stop exits successfully for the long-running ECL launcher"
             (let ((ecl-path (ignore-errors
                               (string-right-trim
                                '(#\Newline #\Return)
                                (uiop:run-program '("bash" "-lc" "command -v ecl")
                                                  :output :string)))))
               (if (or (null ecl-path) (string= ecl-path ""))
                   (ok t "ECL not installed - skipping")
                   (let* ((repo-root (namestring (asdf:system-source-directory :cl-tron-mcp)))
                          (port 4019)
                          (command (format nil
                                           "python - <<'PY'~%
import subprocess, time, urllib.request~%
repo = ~s~%
port = ~d~%
proc = subprocess.Popen(['./start-mcp.sh', '--use-ecl', '--port', str(port)], cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)~%
ready = False~%
for _ in range(30):~%
    if proc.poll() is not None:~%
        break~%
    try:~%
        with urllib.request.urlopen(f'http://127.0.0.1:{port}/health', timeout=1) as response:~%
            if response.read().decode() == '{\"status\":\"ok\"}':~%
                ready = True~%
                break~%
    except Exception:~%
        pass~%
    time.sleep(1)~%
stop = subprocess.run(['./start-mcp.sh', '--stop'], cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, timeout=30)~%
try:~%
    rc = proc.wait(timeout=15)~%
    exited = True~%
except subprocess.TimeoutExpired:~%
    proc.kill()~%
    rc = proc.wait(timeout=5)~%
    exited = False~%
print(f'READY={int(ready)}')~%
print(f'STOP_RC={stop.returncode}')~%
print(f'EXITED={int(exited)}')~%
print(f'PROCESS_RC={rc}')~%
print('---STOP_STDERR---')~%
print(stop.stderr)~%
PY"
                                           repo-root
                                           port))
                          (output (uiop:run-program (list "bash" "-lc" command)
                                                    :output :string)))
                     (ok (search "STOP_RC=0" output) output)
                     (ok (search "EXITED=1" output) output))))))
