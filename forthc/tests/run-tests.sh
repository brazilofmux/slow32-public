#!/bin/bash
# forthc M1 gate: compile hello.fth, run it on every engine, require
# byte-identical program output and a match against hello.expected.
set -u
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FC_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$FC_DIR")"

WORK="$(mktemp -d /tmp/forthc-test.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
FAIL=0

filter() {
    grep -v "^Starting\|^HALT\|^Program halted\|^Exit code\|^Instructions\|^Simulated\|^Cycles\|^Wall time\|^Performance\|instructions/second" \
    | awk '{l[NR]=$0} END{n=NR; while(n>0 && l[n]==""){n--}; for(i=1;i<=n;i++) print l[i]}'
}

if ! bash "$FC_DIR/compile.sh" "$SCRIPT_DIR/hello.fth" "$WORK/hello.s32x" >/dev/null 2>&1; then
    echo "FAIL: compile"
    exit 1
fi

FIRST=""
for e in "$ROOT/tools/emulator/slow32" \
         "$ROOT/tools/emulator/slow32-fast" \
         "$ROOT/tools/dbt/slow32-dbt"; do
    [ -x "$e" ] || continue
    n="$(basename "$e")"
    "$e" "$WORK/hello.s32x" 2>/dev/null | filter > "$WORK/out.$n"
    if [ -z "$FIRST" ]; then
        FIRST="$WORK/out.$n"
        echo "OK:   $n (reference)"
    elif cmp -s "$FIRST" "$WORK/out.$n"; then
        echo "OK:   $n identical"
    else
        echo "FAIL: $n differs"
        FAIL=1
    fi
done

if [ -f "$SCRIPT_DIR/hello.expected" ]; then
    if cmp -s "$FIRST" "$SCRIPT_DIR/hello.expected"; then
        echo "OK:   matches hello.expected"
    else
        echo "FAIL: expected output differs"
        diff "$SCRIPT_DIR/hello.expected" "$FIRST" | head -10
        FAIL=1
    fi
else
    cp "$FIRST" "$SCRIPT_DIR/hello.expected"
    echo "NOTE: seeded hello.expected"
fi

[ "$FAIL" = 0 ] && echo "forthc: PASS" || echo "forthc: FAIL"
exit $FAIL
