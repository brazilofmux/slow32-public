#!/bin/bash
# forthc M4: the differential. Each kernel test in forth/tests is run
# two ways — interpreted by the DTC kernel, and compiled by forthc —
# and the outputs must match byte for byte. Tests whose vocabulary is
# outside the closed world SKIP with the compiler's reason.
set -u
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FC_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$FC_DIR")"
EMU="$ROOT/tools/emulator/slow32-fast"
KERNEL="$ROOT/forth/kernel.s32x"
PRELUDE="$ROOT/forth/prelude.fth"

WORK="$(mktemp -d /tmp/forthc-diff.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
PASS=0; SKIP=0; FAIL=0

filter() {
    sed 's/ok> //g' \
    | grep -v "^Starting\|^SLOW-32 Forth\|^MMIO enabled\|^HALT \|^Program halted\|^Exit code\|^Instructions\|^Simulated\|^Cycles\|^Wall time\|^Performance\|instructions/second" \
    | sed 's/[[:space:]]*$//' \
    | awk '{l[NR]=$0} END{n=NR; while(n>0 && l[n]==""){n--}; for(i=1;i<=n;i++) print l[i]}'
}

for t in "$ROOT"/forth/tests/test-*.fth; do
    name="$(basename "$t" .fth)"
    if ! bash "$FC_DIR/compile.sh" "$t" "$WORK/$name.s32x" \
            > "$WORK/$name.cc.log" 2>&1; then
        reason="$(grep -m1 "forthc:" "$WORK/$name.cc.log" | sed 's/forthc: //')"
        printf "SKIP: %-28s (%s)\n" "$name" "${reason:-compile failed}"
        SKIP=$((SKIP+1))
        continue
    fi
    "$EMU" "$WORK/$name.s32x" 2>/dev/null | filter > "$WORK/$name.compiled"
    cat "$PRELUDE" "$t" | "$EMU" "$KERNEL" 2>/dev/null | filter > "$WORK/$name.dtc"
    if cmp -s "$WORK/$name.compiled" "$WORK/$name.dtc"; then
        printf "OK:   %s\n" "$name"
        PASS=$((PASS+1))
    else
        printf "FAIL: %s\n" "$name"
        diff "$WORK/$name.dtc" "$WORK/$name.compiled" | head -6
        FAIL=$((FAIL+1))
    fi
done

echo "differential: $PASS matched, $SKIP skipped, $FAIL diverged"
[ "$FAIL" = 0 ] || exit 1
