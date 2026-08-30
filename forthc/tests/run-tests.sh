#!/bin/bash
# forthc gate: compile each tests/*.fth, run on every engine, require
# byte-identical output across engines and a match to <name>.expected.
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

for src in "$SCRIPT_DIR"/*.fth; do
    name="$(basename "$src" .fth)"
    if ! bash "$FC_DIR/compile.sh" "$src" "$WORK/$name.s32x" >/dev/null 2>&1; then
        echo "FAIL: $name (compile)"
        FAIL=1
        continue
    fi
    FIRST=""
    for e in "$ROOT/tools/emulator/slow32" \
             "$ROOT/tools/emulator/slow32-fast" \
             "$ROOT/tools/dbt/slow32-dbt"; do
        [ -x "$e" ] || continue
        en="$(basename "$e")"
        "$e" "$WORK/$name.s32x" 2>/dev/null | filter > "$WORK/$name.$en"
        if [ -z "$FIRST" ]; then
            FIRST="$WORK/$name.$en"
        elif ! cmp -s "$FIRST" "$WORK/$name.$en"; then
            echo "FAIL: $name ($en diverges)"
            FAIL=1
        fi
    done
    exp="$SCRIPT_DIR/$name.expected"
    if [ -f "$exp" ]; then
        if cmp -s "$FIRST" "$exp"; then
            echo "OK:   $name"
        else
            echo "FAIL: $name (expected differs)"
            diff "$exp" "$FIRST" | head -8
            FAIL=1
        fi
    else
        cp "$FIRST" "$exp"
        echo "SEED: $name.expected"
    fi
done

[ "$FAIL" = 0 ] && echo "forthc: PASS" || echo "forthc: FAIL"
exit $FAIL
