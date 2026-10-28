#!/bin/bash
# probe.sh -- judge stage08 cc against C11 (ISO/IEC 9899:2011, read from
# WG14's N1570), one small program at a time.
#
#   ./probe.sh probes/6.7.5-alignas.c ...     # named probes
#   ./probe.sh                                # every probe under probes/
#   ./probe.sh -v probes/x.c                  # show stage08's error and the diff
#
# A probe is a C program whose first line names the clause it exercises:
#
#   // 6.7.5p3  _Alignas on an object raises its alignment
#
# clang (the in-tree LLVM, -std=c11 -pedantic-errors) is the oracle for
# whether the program is valid C11, and both builds run on the reference
# interpreter.  A probe that main() checks itself should print a line and
# return 0; any output works, since the two builds' stdout and exit status
# are compared.  `// expect: reject` on a later line marks a program that
# violates a constraint or syntax rule, and the oracle must agree.
#
# Verdicts (one line per probe):
#   PASS     valid; stage08 builds it and prints what clang's build prints
#   REJECTS  valid; stage08 refuses it                       -- a gap
#   WRONG    valid; both build, the results differ           -- wrong code
#   DIAG     invalid; stage08 refuses it, as it must
#   ACCEPTS  invalid; stage08 takes it without a word        -- a gap
#   ORACLE   `expect: reject` and clang disagree: fix the probe
#   HANG     a build or run passed its time limit
#
# stage08 here is the tree's: selfhost/stage08/cc.s32x, its s32-as/s32-ld,
# include/ and lib/ (archived once per run).  Exit status: 0 when every
# probe is PASS or DIAG.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
S08="$ROOT/selfhost/stage08"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
DBT="$ROOT/tools/dbt/slow32-dbt"
REF="$ROOT/tools/emulator/slow32"
LIMIT="${LIMIT:-60}"

VERBOSE=0
if [ "${1:-}" = "-v" ]; then VERBOSE=1; shift; fi

for f in "$S08/cc.s32x" "$S08/s32-as.s32x" "$S08/s32-ld.s32x" "$S08/s32-ar.s32x" \
         "$LLVM_BIN/clang" "$DBT" "$REF"; do
    [ -e "$f" ] || { echo "missing: $f (build stage08 with make, and the tools with make)" >&2; exit 2; }
done

W="$(mktemp -d "${TMPDIR:-/tmp}/c-conformance.XXXXXX")"
trap '[ "${KEEP:-0}" = 1 ] && echo "kept: $W" >&2 || rm -rf "$W"' EXIT

# stage08's libc, as the kit ships it: every lib/ object but crt0 in one archive
LIBC_OBJS=()
for o in "$S08"/lib/*.s32o; do
    [ "$(basename "$o")" = crt0.s32o ] || LIBC_OBJS+=("$o")
done
"$DBT" "$S08/s32-ar.s32x" rc "$W/libc.s32a" "${LIBC_OBJS[@]}" >/dev/null 2>&1 \
    || { echo "could not archive stage08/lib" >&2; exit 2; }

run_limited() {   # run_limited OUT_FILE cmd...  -> the command's status, 124 on timeout
    local out="$1"; shift
    timeout "$LIMIT" "$@" >"$out" 2>&1
}

if [ $# -eq 0 ]; then
    set -- "$HERE"/probes/*.c
fi

fail=0
for src in "$@"; do
    name="$(basename "$src" .c)"
    clause="$(head -1 "$src" | sed -n 's|^// *||p')"
    expect_reject=0
    grep -q '^// expect: reject' "$src" && expect_reject=1
    d="$W/$name"; mkdir -p "$d"

    # clang, the oracle
    SLOW32_HOME="$ROOT" "$LLVM_BIN/clang" --target=slow32-unknown-none -std=c11 -pedantic-errors \
        -O1 "$src" -o "$d/clang.s32x" >"$d/clang.log" 2>&1
    clang_ok=$?

    # stage08
    s08_ok=1; s08_why=""
    run_limited "$d/cc.log" "$DBT" "$S08/cc.s32x" -I"$S08/include" "$src" "$d/s08.s"
    rc=$?
    if [ $rc -eq 124 ]; then s08_ok=124; s08_why="compiler timed out"
    elif [ $rc -ne 0 ] || [ ! -s "$d/s08.s" ]; then s08_ok=1
        s08_why="$(grep -m1 -E 'error|expected' "$d/cc.log" | cut -c1-110)"
    elif ! "$DBT" "$S08/s32-as.s32x" "$d/s08.s" "$d/s08.s32o" >"$d/as.log" 2>&1 || [ ! -s "$d/s08.s32o" ]; then
        s08_ok=1; s08_why="assembler: $(grep -m1 -i error "$d/as.log" | cut -c1-90)"
    elif ! "$DBT" "$S08/s32-ld.s32x" -o "$d/s08.s32x" --mmio 64K "$S08/lib/crt0.s32o" "$d/s08.s32o" \
            "$W/libc.s32a" >"$d/ld.log" 2>&1 || [ ! -s "$d/s08.s32x" ]; then
        s08_ok=1; s08_why="linker: $(grep -m1 -i -E 'error|undefined' "$d/ld.log" | cut -c1-90)"
    else
        s08_ok=0
    fi

    verdict=""; note=""
    if [ $s08_ok -eq 124 ]; then
        verdict=HANG; note="$s08_why"
    elif [ $expect_reject -eq 1 ] && [ $clang_ok -eq 0 ]; then
        verdict=ORACLE; note="marked reject, clang accepts it"
    elif [ $expect_reject -eq 0 ] && [ $clang_ok -ne 0 ]; then
        verdict=ORACLE; note="clang rejects it: $(grep -m1 error "$d/clang.log" | sed 's|.*error: ||' | cut -c1-80)"
    elif [ $clang_ok -ne 0 ]; then
        if [ $s08_ok -eq 0 ]; then verdict=ACCEPTS; note="clang: $(grep -m1 error "$d/clang.log" | sed 's|.*error: ||' | cut -c1-80)"
        else verdict=DIAG; note="$s08_why"; fi
    elif [ $s08_ok -ne 0 ]; then
        verdict=REJECTS; note="$s08_why"
    else
        run_limited "$d/clang.out" "$REF" "$d/clang.s32x"; crc=$?
        run_limited "$d/s08.out" "$REF" "$d/s08.s32x"; src_rc=$?
        if [ $crc -eq 124 ] || [ $src_rc -eq 124 ]; then
            verdict=HANG; note="run timed out (clang $crc, stage08 $src_rc)"
        else
            # the interpreter's banner and counters are not the program's
            for x in clang s08; do
                grep -v -E '^(Starting execution|Program halted\.|Instructions executed:|Cycles:|Wall time:|Performance:|HALT at PC|Exit code:|MMIO enabled)' \
                    "$d/$x.out" | sed '/^$/d' >"$d/$x.txt"
            done
            if [ $crc -eq $src_rc ] && cmp -s "$d/clang.txt" "$d/s08.txt"; then verdict=PASS
            else verdict=WRONG; note="exit clang $crc / stage08 $src_rc; $(diff "$d/clang.txt" "$d/s08.txt" | grep -m1 '^[<>]' | cut -c1-80)"
            fi
        fi
    fi

    case $verdict in PASS|DIAG) ;; *) fail=1 ;; esac
    printf "%-8s %-36s %s\n" "$verdict" "$name" "${note:-$clause}"
    if [ $VERBOSE = 1 ] && [ "$verdict" != PASS ]; then
        [ -s "$d/cc.log" ] && { echo "  -- stage08:"; grep -v '^\s*$' "$d/cc.log" | grep -i -E 'error|expected|warn' | head -5 | sed 's/^/     /'; }
        [ -s "$d/clang.log" ] && { echo "  -- clang:"; head -5 "$d/clang.log" | sed 's/^/     /'; }
        [ -f "$d/s08.txt" ] && diff "$d/clang.txt" "$d/s08.txt" | head -8 | sed 's/^/     /'
    fi
done
exit $fail
