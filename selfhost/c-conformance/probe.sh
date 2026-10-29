#!/bin/bash
# probe.sh -- judge stage08 cc against C11 (ISO/IEC 9899:2011, read from
# WG14's N1570), one small program at a time.
#
#   ./probe.sh probes/6.7.5-alignas.c ...     # named probes
#   ./probe.sh                                # every probe under probes/
#   ./probe.sh -v probes/x.c                  # show stage08's error and the diff
#   ./probe.sh --check                        # every probe against verdicts.txt
#   ./probe.sh --update                       # record this run as verdicts.txt
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
# `// expect: warnings` marks a valid program on which clang's default
# warnings fire (not -pedantic-errors: they are not errors there): the
# lines stage08 warns on must be the lines clang does, or the probe is
# WARNS -- the ruling has stage08 warn where clang's defaults warn.
#
# Verdicts (one line per probe):
#   PASS     valid; stage08 builds it and prints what clang's build prints
#   REJECTS  valid; stage08 refuses it                       -- a gap
#   WRONG    valid; both build, the results differ           -- wrong code
#   DIAG     invalid; stage08 refuses it, as it must
#   ACCEPTS  invalid; stage08 takes it without a word        -- a gap
#   ORACLE   `expect: reject` and clang disagree: fix the probe
#   OPTIONAL a probe of an optional C11 feature (`// optional: __STDC_NO_X__`)
#            that stage08 declares it leaves out, which it then refuses or
#            answers otherwise than clang, which has it -- a choice, not a gap
#   HANG     a build or run passed its time limit
#   WARNS    `expect: warnings`, PASS but for warnings on other lines
#
# stage08 here is the tree's: selfhost/stage08/cc.s32x, its s32-as/s32-ld,
# include/ and lib/ (archived once per run).  Exit status: 0 when every
# probe is PASS or DIAG.
#
# verdicts.txt holds each probe's last recorded verdict, so the survey is
# also a regression gate while the gaps are closed: --check runs every
# probe and fails if one that was PASS or DIAG is now anything else, and
# names the ones that got better (run --update then, and commit it).
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
S08="$ROOT/selfhost/stage08"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
DBT="$ROOT/tools/dbt/slow32-dbt"
REF="$ROOT/tools/emulator/slow32"
LIMIT="${LIMIT:-60}"

VERBOSE=0
MODE=run
case "${1:-}" in
    -v) VERBOSE=1; shift ;;
    --check) MODE=check; shift ;;
    --update) MODE=update; shift ;;
esac

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

s08_defines() {   # s08_defines MACRO -> 0 when stage08 predefines it
    printf '#ifndef %s\n#error not defined\n#endif\nint s08_defines_probe;\n' "$1" >"$W/defines.c"
    "$DBT" "$S08/cc.s32x" -I"$S08/include" "$W/defines.c" "$W/defines.s" >/dev/null 2>&1
}
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
    expect_warn=0
    grep -q '^// expect: warnings' "$src" && expect_warn=1
    optional="$(sed -n 's|^// optional: *\([A-Za-z_0-9]*\).*|\1|p' "$src" | head -1)"
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

    if [ "$verdict" = PASS ] && [ $expect_warn -eq 1 ]; then
        # the lines each warns on, in the probe itself (not in a header)
        SLOW32_HOME="$ROOT" "$LLVM_BIN/clang" --target=slow32-unknown-none -std=c11 -fsyntax-only \
            "$src" 2>&1 | sed -n -E "s|^.*$name\.c:([0-9]+):[0-9]+: warning: .*|\1|p" | sort -n >"$d/clang.warn"
        sed -n -E "s|^.*$name\.c:([0-9]+): warning: .*|\1|p" "$d/cc.log" | sort -n >"$d/s08.warn"
        if ! cmp -s "$d/clang.warn" "$d/s08.warn"; then
            verdict=WARNS
            note="warning lines: clang $(tr '\n' ' ' <"$d/clang.warn")/ stage08 $(tr '\n' ' ' <"$d/s08.warn")"
        fi
    fi
    if [ -n "$optional" ] && { [ "$verdict" = WRONG ] || [ "$verdict" = REJECTS ]; } && s08_defines "$optional"; then
        verdict=OPTIONAL; note="stage08 defines $optional"
    fi
    case $verdict in PASS|DIAG|OPTIONAL) ;; *) fail=1 ;; esac
    echo "$name $verdict" >>"$W/verdicts.now"
    [ $MODE = run ] && printf "%-8s %-36s %s\n" "$verdict" "$name" "${note:-$clause}"
    if [ $VERBOSE = 1 ] && [ "$verdict" != PASS ]; then
        [ -s "$d/cc.log" ] && { echo "  -- stage08:"; grep -v '^\s*$' "$d/cc.log" | grep -i -E 'error|expected|warn' | head -5 | sed 's/^/     /'; }
        [ -s "$d/clang.log" ] && { echo "  -- clang:"; head -5 "$d/clang.log" | sed 's/^/     /'; }
        [ -f "$d/s08.txt" ] && diff "$d/clang.txt" "$d/s08.txt" | head -8 | sed 's/^/     /'
    fi
done

good() { case "$1" in PASS|DIAG|OPTIONAL) return 0 ;; *) return 1 ;; esac; }
if [ $MODE = update ]; then
    sort "$W/verdicts.now" >"$HERE/verdicts.txt"
    awk '{print $2}' "$HERE/verdicts.txt" | sort | uniq -c | sort -rn
    exit 0
fi
if [ $MODE = check ]; then
    worse=0; better=0
    while read -r name now; do
        was="$(awk -v n="$name" '$1 == n {print $2}' "$HERE/verdicts.txt")"
        if [ -z "$was" ]; then echo "NEW      $name $now"; continue; fi
        [ "$was" = "$now" ] && continue
        if good "$was" && ! good "$now"; then echo "WORSE    $name $was -> $now"; worse=$((worse + 1))
        elif good "$now"; then echo "BETTER   $name $was -> $now"; better=$((better + 1))
        else echo "CHANGED  $name $was -> $now"; fi
    done < <(sort "$W/verdicts.now")
    awk '{print $2}' "$W/verdicts.now" | sort | uniq -c | sort -rn | tr '\n' ' '; echo
    echo "c-conformance: $worse worse, $better better than verdicts.txt"
    [ $worse -eq 0 ]; exit $?
fi
exit $fail
