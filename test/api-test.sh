#!/usr/bin/env bash
# Smoke test for the Math Training Machine API.
# Usage: ./test/api-test.sh [base-url]
set -u
BASE="${1:-http://127.0.0.1:8123}"
fail=0

check() { # name expected actual
  if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected [$2] got [$3]"; fail=1; fi
}

echo "== $BASE =="

code=$(curl -s -o /tmp/mtm-health.json -w '%{http_code}' "$BASE/api/health")
check "health status" "200" "$code"
check "health body" '{"ok":true,"service":"math-training-machine"}' "$(cat /tmp/mtm-health.json)"

code=$(curl -s -o /tmp/mtm-index.html -w '%{http_code}' "$BASE/")
check "index status" "200" "$code"
grep -q 'id="app"' /tmp/mtm-index.html && echo "ok   index has #app" || { echo "FAIL index"; fail=1; }

code=$(curl -s -o /tmp/mtm-main.js -w '%{http_code}' "$BASE/js/main.js")
check "main.js status" "200" "$code"
grep -q 'mtm' /tmp/mtm-main.js && echo "ok   main.js served" || { echo "FAIL main.js content"; fail=1; }

code=$(curl -s -o /tmp/mtm-style.css -w '%{http_code}' "$BASE/css/style.css")
check "style.css status" "200" "$code"

code=$(curl -s -o /tmp/mtm-traversal -w '%{http_code}' --path-as-is "$BASE/../../../etc/passwd")
check "traversal blocked" "403" "$code"

code=$(curl -s -o /tmp/mtm-badmode -w '%{http_code}' "$BASE/api/leaderboard?mode=nope")
check "bad mode" "400" "$code"

code=$(curl -s -o /tmp/mtm-post-bad -w '%{http_code}' -X POST \
  -H 'content-type: application/json' -d '{"name":"","mode":"sum","score":10}' \
  "$BASE/api/leaderboard")
check "empty name rejected" "400" "$code"

# a valid score, plus one that must rank above it
code=$(curl -s -o /tmp/mtm-post1.json -w '%{http_code}' -X POST \
  -H 'content-type: application/json' \
  -d '{"name":"ada","mode":"sum","score":1240,"correct":14,"total":16,"best-streak":9,"avg-ms":1450}' \
  "$BASE/api/leaderboard")
check "post low score" "201" "$code"

code=$(curl -s -o /tmp/mtm-post2.json -w '%{http_code}' -X POST \
  -H 'content-type: application/json' \
  -d '{"name":"grace","mode":"sum","score":2400,"correct":18,"total":19,"best-streak":14,"avg-ms":900}' \
  "$BASE/api/leaderboard")
check "post high score" "201" "$code"

code=$(curl -s -o /tmp/mtm-board.json -w '%{http_code}' "$BASE/api/leaderboard?mode=sum&limit=20")
check "board status" "200" "$code"

python3 - <<'PY'
import json, sys
try:
    board = json.load(open('/tmp/mtm-board.json'))
    entries = board['entries']
except Exception as e:
    print("FAIL board json:", e); sys.exit(1)
assert board['mode'] == 'sum', board
names = [e['name'] for e in entries]
assert 'grace' in names and 'ada' in names, names
assert names.index('grace') < names.index('ada'), names
first = entries[0]
for key in ('rank','name','score','correct','total','accuracy','best-streak','avg-ms','ts','id','mode'):
    assert key in first, (key, first)
assert first['score'] >= entries[-1]['score'], 'not sorted'
post = json.load(open('/tmp/mtm-post1.json'))
assert post.get('rank') >= 1, post
print("ok   board ordering and fields")
print("ok   reported rank:", post.get('rank'))
PY

exit $fail
