#!/usr/bin/env bash
# Boots a throwaway server on a spare port with a fresh database, runs the
# API smoke test against it and shuts it down again.
set -u
cd "$(dirname "$0")/.."

PORT="${MTM_TEST_PORT:-8765}"
export MTM_PORT="$PORT"
export MTM_DATA="$(mktemp -d /tmp/mtm-test-data-XXXXXX)"
export MTM_PUBLIC="$PWD/public"
LOG="$(mktemp /tmp/mtm-test-server-XXXXXX.log)"

sbcl --non-interactive --load server/run.lisp > "$LOG" 2>&1 &
PID=$!
cleanup() {
  kill "$PID" 2>/dev/null
  wait "$PID" 2>/dev/null
}
trap cleanup EXIT

ready=0
for _ in $(seq 1 60); do
  if curl -s -m 2 "http://127.0.0.1:$PORT/api/health" > /dev/null 2>&1; then
    ready=1
    break
  fi
  if ! kill -0 "$PID" 2>/dev/null; then
    echo "server exited early, log:"
    cat "$LOG"
    exit 1
  fi
  sleep 1
done

if [ "$ready" != 1 ]; then
  echo "server did not come up, log:"
  cat "$LOG"
  exit 1
fi

./test/api-test.sh "http://127.0.0.1:$PORT"
STATUS=$?

echo
echo "--- server log tail ---"
tail -5 "$LOG"
exit $STATUS
