#!/bin/bash
# Run Free Pascal's own test suite (~/fpc/tests: test/, tbs/, webtbs/) against
# ppcs32 + the SLOW-32 RTL on the emulator, and SORT the outcomes.  Each test
# is a self-checking program: exit 0 is a pass unless {%RESULT=n} says
# otherwise.  Directives honoured: FAIL (compile must fail -> run as a
# negative compile test), NORUN (compile only), CPU/SKIPCPU, TARGET/SKIPTARGET
# (slow32 / embedded), OPT (extra compiler options), RESULT, INTERACTIVE,
# GRAPH, NEEDLIBRARY, FILES (skipped), KNOWNRUNERROR, KNOWNCOMPILEERROR.
#
# Outcomes, one line per test in the TSV ($OUT/results.tsv):
#   PASS COMPILE-FAIL RUN-FAIL(rc) RUN-FAULT ASM-FAIL LINK-FAIL TIMEOUT
#   SKIP(reason) KNOWN  -- COMPILE-FAIL lines carry the first error message,
#   so "Can't find unit X" (RTL coverage) sorts apart from compiler errors
#   and "Internal error" (compiler bugs).
#
# Usage: run-fpc-tests.sh [-j N] [--dirs "test tbs webtbs"] [--limit N]
#                         [--match glob] [--out DIR] [--list FILE]
# Links with --stack-size 1M --heap-size 32M: the linker defaults (64K, 1M)
# showed up as faults just under 0x0FFF0000 and as runtime error 203.
#   env: FPC_SRC (~/fpc), PPCS32, UNITS (fpc-backend/runtime/units), EMU
set -uo pipefail
export LC_ALL=C
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKEND="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT="$(cd "$BACKEND/.." && pwd)"
FPC_SRC="${FPC_SRC:-$HOME/fpc}"
PPCS32="${PPCS32:-$FPC_SRC/compiler/ppcs32}"
UNITS="${UNITS:-$BACKEND/runtime/units}"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
JOBS=8; DIRS="test tbs webtbs"; LIMIT=0; MATCH="t*"; OUT="${OUT:-/tmp/fpc-tests-slow32}"; LIST_IN=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        -j) JOBS="$2"; shift 2 ;;
        --dirs) DIRS="$2"; shift 2 ;;
        --limit) LIMIT="$2"; shift 2 ;;
        --match) MATCH="$2"; shift 2 ;;
        --out) OUT="$2"; shift 2 ;;
        --list) LIST_IN="$2"; shift 2 ;;      # file of test paths (relative to tests/ or absolute): rerun just these
        *) echo "unknown option $1" >&2; exit 2 ;;
    esac
done
for f in "$PPCS32" "$UNITS/system.ppu" "$UNITS/system.s32o" "$UNITS/fpcrtl.s32a" "$AS" "$LD" "$EMU" "$BACKEND/runtime/fpc_startup.s"; do
    [[ -e "$f" ]] || { echo "missing: $f (run build-rtl.sh first?)" >&2; exit 1; }
done
mkdir -p "$OUT/work"
"$AS" "$BACKEND/runtime/fpc_startup.s" "$OUT/fpc_startup.s32o" >/dev/null 2>&1 || { echo "cannot assemble fpc_startup"; exit 1; }
export PPCS32 UNITS EMU AS LD RT OUT
# one test: prints "<name>\t<outcome>\t<detail>"
run_one() {
    src="$1"; name="$(basename "$src" .pp)"; rel="${src#$FPC_SRC/tests/}"
    w="$OUT/work/$name"; rm -rf "$w"; mkdir -p "$w"
    head="$(head -c 3000 "$src")"
    dir() { printf '%s' "$head" | grep -io "{ *%$1[^}]*}" | head -1 | sed -E "s/\{ *%$1 *=? *//I; s/ *\}//"; }
    has() { printf '%s' "$head" | grep -iq "{ *%$1\b"; }
    skip=""
    # a t*.pp that is a unit is a helper for another test, not a test
    if sed -E 's#\{[^}]*\}##g; s#//.*##' "$src" | grep -m1 -iE '^[[:space:]]*(unit|library|program)\b' | grep -qiE '^[[:space:]]*(unit|library)\b'; then skip="unit/library"; fi
    if has INTERACTIVE; then skip="interactive"; fi
    if has GRAPH; then skip="graph"; fi
    if has NEEDLIBRARY; then skip="needlibrary"; fi
    if has FILES; then skip="needs files"; fi
    cpu="$(dir CPU)"; if [[ -n "$cpu" ]] && ! echo "$cpu" | grep -qiw slow32; then skip="cpu=$cpu"; fi
    scpu="$(dir SKIPCPU)"; if [[ -n "$scpu" ]] && echo "$scpu" | grep -qiw slow32; then skip="skipcpu"; fi
    tgt="$(dir TARGET)"; if [[ -n "$tgt" ]] && ! echo "$tgt" | grep -qiw embedded; then skip="target=$tgt"; fi
    stgt="$(dir SKIPTARGET)"; if [[ -n "$stgt" ]] && echo "$stgt" | grep -qiw embedded; then skip="skiptarget"; fi
    if has RECOMPILE; then skip="recompile"; fi
    if [[ -n "$skip" ]]; then printf '%s\tSKIP\t%s\n' "$rel" "$skip"; return; fi
    opt="$(dir OPT)"; expect="$(dir RESULT)"; [[ -z "$expect" ]] && expect=0
    kre="$(dir KNOWNRUNERROR)"; kce="$(dir KNOWNCOMPILEERROR)"
    # compile (the test's own directory is an include/unit path: helper units live there)
    ( cd "$w" && timeout 120 "$PPCS32" -Tembedded -s -n $opt -Fu"$UNITS" -Fu"$(dirname "$src")" -Fi"$(dirname "$src")" -Fu"$FPC_SRC/tests/tstunits" -Fi"$FPC_SRC/tests/tstunits" -FE"$w" "$src" ) > "$w/compile.log" 2>&1
    crc=$?
    if has FAIL; then
        if [[ $crc -ne 0 ]]; then printf '%s\tPASS\tcompile failed as required\n' "$rel"; else printf '%s\tNEG-FAIL\tcompiled but %%FAIL\n' "$rel"; fi
        return
    fi
    if [[ $crc -ne 0 ]]; then
        msg="$(grep -m1 -E 'Error|Fatal' "$w/compile.log" | sed -E 's/^[^ ]*\([0-9,]+\) //' | cut -c1-120)"
        if [[ -n "$kce" ]]; then printf '%s\tKNOWN\tcompile: %s\n' "$rel" "$msg"; else printf '%s\tCOMPILE-FAIL\t%s\n' "$rel" "${msg:-rc=$crc}"; fi
        return
    fi
    if has NORUN; then printf '%s\tPASS\tcompiled (norun)\n' "$rel"; return; fi
    objs=""
    for s in "$w"/*.s; do
        [[ -e "$s" ]] || continue
        "$AS" "$s" "${s%.s}.s32o" > "$w/as.log" 2>&1 || { printf '%s\tASM-FAIL\t%s\n' "$rel" "$(grep -m1 -i error "$w/as.log" | cut -c1-120)"; return; }
        objs="$objs ${s%.s}.s32o"
    done
    "$LD" --mmio 64K --stack-size 1M --heap-size 32M -o "$w/t.s32x" "$RT/crt0.s32o" "$OUT/fpc_startup.s32o" "$UNITS/system.s32o" $objs "$UNITS/fpcrtl.s32a" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$w/ld.log" 2>&1 \
        || { printf '%s\tLINK-FAIL\t%s\n' "$rel" "$(grep -m1 -i 'error\|undefined' "$w/ld.log" | cut -c1-120)"; return; }
    ( cd "$w" && timeout 60 "$EMU" t.s32x < /dev/null > run.log 2>&1 ); rrc=$?
    if [[ $rrc -eq 124 ]]; then printf '%s\tTIMEOUT\t\n' "$rel"; return; fi
    if grep -q 'fault\|Error: \(Read\|Write\|Execute\)\|Memory fault' "$w/run.log"; then printf '%s\tRUN-FAULT\t%s\n' "$rel" "$(grep -m1 'fault\|Error:' "$w/run.log" | cut -c1-100)"; return; fi
    if [[ $rrc -eq $expect ]]; then printf '%s\tPASS\t\n' "$rel"; rm -rf "$w"
    elif [[ -n "$kre" ]]; then printf '%s\tKNOWN\trun rc=%s\n' "$rel" "$rrc"
    else printf '%s\tRUN-FAIL\trc=%s expected %s: %s\n' "$rel" "$rrc" "$expect" "$(grep -v '^Starting\|^MMIO\|^HALT\|^Program halted\|^Instructions\|^Simulated\|^Wall\|^Perf\|instructions/second\|^Exit code\|^$' "$w/run.log" | tail -1 | cut -c1-80)"; fi
}
export -f run_one
list="$OUT/list.txt"; : > "$list"
if [[ -n "$LIST_IN" ]]; then sed -E "s#^([^/])#$FPC_SRC/tests/\\1#" "$LIST_IN" > "$list"; else
for d in $DIRS; do ls "$FPC_SRC/tests/$d"/$MATCH.pp 2>/dev/null >> "$list"; done; fi
[[ $LIMIT -gt 0 ]] && { head -n "$LIMIT" "$list" > "$list.l"; mv "$list.l" "$list"; }
total=$(wc -l < "$list" | tr -d ' ')
echo "running $total tests, $JOBS at a time, results in $OUT/results.tsv"
export FPC_SRC
xargs -P "$JOBS" -I{} bash -c 'run_one "$1"' _ {} < "$list" > "$OUT/results.tsv"
echo "=== outcomes:"; cut -f2 "$OUT/results.tsv" | sort | uniq -c | sort -rn
echo "=== compile failures by message:"; awk -F'\t' '$2=="COMPILE-FAIL"{print $3}' "$OUT/results.tsv" | sed -E 's/unit [A-Za-z0-9_]+ used by.*/unit X used by .../' | sort | uniq -c | sort -rn | head -15
echo "=== skips by reason:"; awk -F'\t' '$2=="SKIP"{print $3}' "$OUT/results.tsv" | sed -E 's/=.*//' | sort | uniq -c | sort -rn | head -8
