#!/usr/bin/env bash
# Diagnose ecl-combined-startup-stays-running-test: run the same launch the test does and report
# what happens second by second, instead of just "exit code 1".
#
# Usage (from anywhere):  scripts/diagnose-ecl-startup.sh [port] [max-seconds]
# Paste the whole output into the report; the last lines say which case you are in.
set -u
cd "$(dirname "$0")/.." || exit 2
port="${1:-4018}"
max="${2:-90}"
out=$(mktemp) ; err=$(mktemp)
echo "== launching: ./start-mcp.sh --use-ecl --port $port  (stdout=$out stderr=$err)"
start=$(date +%s)
./start-mcp.sh --use-ecl --port "$port" >"$out" 2>"$err" &
pid=$!
result=TIMEOUT
for i in $(seq 1 "$max"); do
  if ! kill -0 "$pid" 2>/dev/null; then
    wait "$pid"; status=$?
    echo "== t=$(( $(date +%s) - start ))s launcher EXITED by itself with status $status"
    result=EXITED
    break
  fi
  body=$(curl -s -m 2 "http://127.0.0.1:$port/health" || true)
  if [ "$body" = '{"status":"ok"}' ]; then
    echo "== t=$(( $(date +%s) - start ))s /health answered: $body"
    result=ALIVE
    break
  fi
  [ $(( i % 10 )) -eq 0 ] && echo "== t=${i}s still waiting (no /health yet)"
  sleep 1
done
echo "== stopping launcher (pid $pid)"
kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null; echo "== launcher status after SIGTERM: $?"
echo "== RESULT: $result"
echo "== last 25 lines of launcher stderr:"
tail -25 "$err"
echo "== last 10 lines of launcher stdout:"
tail -10 "$out"
rm -f "$out" "$err"
