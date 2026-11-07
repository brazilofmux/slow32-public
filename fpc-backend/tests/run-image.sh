#!/bin/bash
# The slow32:pascal image's gate: its s32pc and both RTL variants, run on the
# engines the image has.  shift64 (the soft-float RTL) and frametemps (-O2
# against the -CfSLOW32 RTL) against their expected.txt; fputest under each
# variant must print ALL TESTS PASSED and exit 0.  cdecl-abi needs clang,
# which this image does not carry.
#   docker run --rm -v "$PWD":/data slow32:pascal bash fpc-backend/tests/run-image.sh
# Exit 0 only if every check passes on every engine.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
EX="$(cd "$HERE/../examples" && pwd)"
W="$(mktemp -d "${TMPDIR:-/tmp}/pascal-gate.XXXXXX")"
trap 'rm -rf "${W:?}"' EXIT
ENGINES=("" --fast --qemu)
bad=0
filter() { grep -vE '^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code|Instructions|Simulated|Wall time|Performance|Cycles|=== Running|QEMU|slow32: native|  mem|  str| +[0-9.]+ instructions/second)|^$' | sed 's/^(qemu) //'; }
one() {   # name source expected-file-or-"" s32pc-flags...
    local n="$1" src="$2" exp="$3"; shift 3
    if ! s32pc "$@" "$src" "$W/$n.s32x" >"$W/$n.log" 2>&1; then
        echo "FAIL $n: did not build"; sed -n 1,8p "$W/$n.log"; bad=$((bad + 1)); return
    fi
    for e in "${ENGINES[@]}"; do
        s32run $e "$W/$n.s32x" >"$W/$n.raw" 2>&1; rc=$?
        filter <"$W/$n.raw" >"$W/$n.out"
        if [ -n "$exp" ]; then
            [ "$n" = shift64 ] && { grep -E '^i?sh[lr] ' "$W/$n.out" >"$W/$n.f"; mv "$W/$n.f" "$W/$n.out"; }
            if diff -q "$exp" "$W/$n.out" >/dev/null; then echo "ok   $n ${e:-dbt}"
            else echo "FAIL $n ${e:-dbt}"; diff "$exp" "$W/$n.out" | head -5; bad=$((bad + 1)); fi
        elif [ $rc -eq 0 ] && grep -q 'ALL TESTS PASSED' "$W/$n.out"; then echo "ok   $n ${e:-dbt}"
        else echo "FAIL $n ${e:-dbt} (rc $rc)"; grep FAIL "$W/$n.out" | head -5; bad=$((bad + 1)); fi
    done
}
one shift64 "$HERE/shift64/shift64.pas" "$HERE/shift64/expected.txt"
one frametemps "$HERE/frametemps/frametemps.pas" "$HERE/frametemps/expected.txt" --fpu -O2
one fputest "$EX/fputest.pas" ""
one fputest-fpu "$EX/fputest.pas" "" --fpu
echo "pascal image: $bad failed"
[ $bad -eq 0 ]
