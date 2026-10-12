#!/bin/bash
# islisp/ gate.
#
#   tests/run-tests.sh            everything below
#   tests/run-tests.sh quick      host build over the suite only
#
# 1. verify (host):    the ISLISP verification suite through out/islisp-host
# 2. verify (s32x):    the same through out/islisp.s32x on the DBT
# 3. stage08 identity: every verify file's output from out/islisp-stage08.s32x
#                      byte-identical to the clang build's (when the stage08
#                      build exists; build.sh stage08 makes it)
# 4. programs:         tests/*.lsp on the emulator, byte-diffed against the
#                      oracle (when the oracle image is present), exit 0 on both
#
# The suite lives in ~/refs/eisl/verify, never in the tree.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
IDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$IDIR/.." && pwd)"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
VERIFY="${VERIFY:-$HOME/refs/eisl/verify}"
FAIL=0
PASS=0

[ -x "$IDIR/out/islisp-host" ] && [ -f "$IDIR/out/islisp.s32x" ] || "$IDIR/build.sh" >/dev/null

echo "=== verify (host) ==="
if OUT=/tmp/islisp-gate-host "$HERE/run-verify.sh" --host | tail -1 | grep -q " 0 fail"; then PASS=$((PASS+1)); echo "  PASS"; else FAIL=$((FAIL+1)); echo "  FAIL"; fi
[ "${1:-}" = quick ] && { echo "islisp: $PASS passed, $FAIL failed"; exit $FAIL; }

echo "=== verify (SLOW-32, clang build) ==="
if OUT=/tmp/islisp-gate-s32x "$HERE/run-verify.sh" --s32x | tail -1 | grep -q " 0 fail"; then PASS=$((PASS+1)); echo "  PASS"; else FAIL=$((FAIL+1)); echo "  FAIL"; fi

echo "=== stage08 identity ==="
if [ -f "$IDIR/out/islisp-stage08.s32x" ]; then
    OUT=/tmp/islisp-gate-stage08 "$HERE/run-verify.sh" --stage08 >/dev/null 2>&1
    diffs=0
    for f in /tmp/islisp-gate-s32x/ours/*.out; do
        b="$(basename "$f")"
        if ! cmp -s "$f" "/tmp/islisp-gate-stage08/ours/$b"; then diffs=$((diffs+1)); echo "  differs: $b"; fi
    done
    if [ "$diffs" -eq 0 ]; then PASS=$((PASS+1)); echo "  PASS (identical over $(ls /tmp/islisp-gate-s32x/ours | wc -l | tr -d ' ') files)"; else FAIL=$((FAIL+1)); echo "  FAIL ($diffs differ)"; fi
else
    echo "  SKIP (no out/islisp-stage08.s32x; ./build.sh stage08)"
fi

echo "=== programs vs the oracle ==="
ORACLE=1
"$HERE/oracle.sh" --check 2>/dev/null || ORACLE=0
W="$(mktemp -d "$HOME/.islisp-tests.XXXXXX")"
trap 'rm -rf "$W"' EXIT
for f in "$HERE"/*.lsp; do
    [ -e "$f" ] || continue
    b="$(basename "$f" .lsp)"
    cp "$f" "$W/$b.lsp"
    (cd "$W" && "$EMU" "$IDIR/out/islisp.s32x" -l "$IDIR/lib" "$b.lsp" > "$W/$b.got" 2>&1); grc=$?
    if [ "$ORACLE" = 1 ]; then
        (cd "$W" && "$HERE/oracle.sh" "$W/$b.lsp" > "$W/$b.want" 2>&1); wrc=$?
        if [ "$grc" -ne 0 ] || [ "$wrc" -ne 0 ]; then printf "  %-20s FAIL (exit ours=%s oracle=%s)\n" "$b" "$grc" "$wrc"; FAIL=$((FAIL+1))
        elif ! cmp -s "$W/$b.want" "$W/$b.got"; then printf "  %-20s FAIL (output differs)\n" "$b"; diff "$W/$b.want" "$W/$b.got" | head -6; FAIL=$((FAIL+1))
        else printf "  %-20s PASS\n" "$b"; PASS=$((PASS+1)); fi
    else
        if [ "$grc" -eq 0 ]; then printf "  %-20s PASS (exit 0; oracle absent, output not diffed)\n" "$b"; PASS=$((PASS+1))
        else printf "  %-20s FAIL (exit %s)\n" "$b" "$grc"; FAIL=$((FAIL+1)); fi
    fi
done

echo "islisp: $PASS passed, $FAIL failed"
exit $FAIL
