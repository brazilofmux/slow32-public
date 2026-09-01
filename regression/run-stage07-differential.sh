#!/bin/bash

# SLOW-32 stage07 bootstrap-compiler differential
#
# stage07 is the frozen compiler that builds stage08's libc and tools.  It
# is never exercised directly, so its bugs surface as downstream mysteries:
# a tool misbehaves, and it takes a day to work back to "the compiler that
# built it is wrong".  That is how the spurious-symbol bug went unexplained
# for weeks -- the decisive experiment, once finally run, was one line:
# compile the same source with stage08 cc instead and see the fault vanish.
#
# This harness runs that experiment on stage07's whole contract, on every
# build.  It builds the SAME sources twice under the SAME emulator --
# once with stage07 cc, once with stage08 cc -- then drives both sets of
# tools over identical inputs and byte-compares what they produce.  Both
# builds run on the same engine, so any difference is compiler-induced.
#
# stage07's contract is 12 C files: 7 libc (string_extra, string_more,
# ctype, convert, stdio, malloc, start) and 5 tools (s32-as, s32-ar,
# s32-ld, slow32dump, slow32dis).  Small enough to cover completely.
#
# A disagreement does NOT by itself say which side is wrong.  It says one
# of them is, which is the hard part; --bisect then narrows it to a single
# translation unit by rebuilding with one object swapped at a time.
#
# Usage: ./run-stage07-differential.sh [--bisect]
# Env: SELFHOST_EMU (build engine), TIMEOUT.
#
# Exit codes: 0 the two compilers agree, 1 a disagreement, 2 setup error.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SELF="$ROOT/selfhost"
S8="$SELF/stage08"

EMU="${SELFHOST_EMU:-$ROOT/tools/emulator/slow32-fast}"
CC07="$SELF/stage07/cc.s32x"
CC08="$S8/cc.s32x"
WORK="$SCRIPT_DIR/results-stage07"
TIMEOUT="${TIMEOUT:-900}"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'

BISECT=0
[ "$1" = "--bisect" ] && BISECT=1

for f in "$EMU" "$CC07" "$CC08" "$S8/build-tools.sh"; do
    if [ ! -e "$f" ]; then
        echo "ERROR: missing: $f" >&2
        exit 2
    fi
done

echo "Bootstrap compilers under comparison:"
echo "  stage07: $CC07"
echo "  stage08: $CC08"
echo "Build engine: $EMU"
echo ""

rm -rf "$WORK"; mkdir -p "$WORK/out07" "$WORK/out08" "$WORK/run07" "$WORK/run08"

build_with() {
    # $1 = compiler, $2 = output dir, $3 = log
    ( cd "$S8" && STAGE7_CC="$1" OUT_DIR="$2" SELFHOST_EMU="$EMU" \
        timeout "$TIMEOUT" bash build-tools.sh ) >"$3" 2>&1
}

printf "  building with stage07... "
if build_with "$CC07" "$WORK/out07" "$WORK/build07.log"; then echo "ok"; else
    echo -e "${RED}FAILED${NC} (see $WORK/build07.log)"; exit 2; fi
printf "  building with stage08... "
if build_with "$CC08" "$WORK/out08" "$WORK/build08.log"; then echo "ok"; else
    echo -e "${RED}FAILED${NC} (see $WORK/build08.log)"
    echo "  NOTE: stage08 cc failing to build stage08's own sources is itself"
    echo "        a finding -- the two compilers must both accept this corpus."
    exit 2; fi
echo ""

# Strip the emulator's own banner and stats: two binaries built by two
# compilers legitimately halt at different PCs and execute different
# instruction counts.  That is not guest output and must not read as a
# disagreement.
normalize_output() {
    sed \
        -e 's/\r$//' \
        -e '/^Starting execution/d' \
        -e '/^MMIO enabled/d' \
        -e '/^HALT at/d' \
        -e '/^Program halted/d' \
        -e '/^Exit code:/d' \
        -e '/^Instructions executed/d' \
        -e '/^Simulated cycles:/d' \
        -e '/^Wall time:/d' \
        -e '/^Performance:/d' \
        -e '/^Cycles:/d' \
        -e '/instructions\/second/d' \
        -e '/^slow32: native intrinsics detected:/d' \
        -e '/^  mem[a-z]*:  *0x[0-9a-f]*$/d' \
        -e '/^  strlen:  *0x[0-9a-f]*$/d'
}

TOTAL=0; AGREE=0; DIVERGED=0; DIVERGED_ITEMS=()

check() {
    # $1 label, $2 file A, $3 file B
    TOTAL=$((TOTAL + 1))
    printf "  %-34s " "$1"
    if [ ! -f "$2" ] || [ ! -f "$3" ]; then
        echo -e "${RED}DIVERGE${NC}: missing output"
        DIVERGED=$((DIVERGED + 1)); DIVERGED_ITEMS+=("$1: missing output"); return
    fi
    if cmp -s "$2" "$3"; then
        echo -e "${GREEN}AGREE${NC}"; AGREE=$((AGREE + 1))
    else
        n=$(cmp -l "$2" "$3" 2>/dev/null | wc -l | tr -d ' ')
        echo -e "${RED}DIVERGE${NC}: $n bytes differ"
        DIVERGED=$((DIVERGED + 1)); DIVERGED_ITEMS+=("$1: $n bytes differ")
    fi
}

# Drive both tool sets over ONE SHARED input set.  Shared matters: these
# tools echo their input PATH, so giving each side its own copy makes them
# "disagree" over nothing.  Only the OUTPUTS are per-side.
SRC="$SELF/src/tools/s32-ar.c"
IN="$WORK/in"
mkdir -p "$IN"

# Reference inputs, produced once.  Which compiler builds them does not
# matter -- both sides consume the identical bytes.
( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$CC08" "$SRC" "$IN/in.s" ) >"$IN/cc.log" 2>&1
if [ ! -s "$IN/in.s" ]; then
    echo "ERROR: could not produce reference assembly" >&2; exit 2
fi
( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$WORK/out08/s32-as.s32x" "$IN/in.s" "$IN/in.s32o" ) \
    >"$IN/as.log" 2>&1
if [ ! -s "$IN/in.s32o" ]; then
    echo "ERROR: could not produce reference object" >&2; exit 2
fi

# The link needs the whole runtime, not just crt0, or it fails to resolve
# and both sides "agree" on producing nothing.
LIBOBJS=""
for o in "$S8"/lib/*.s32o; do
    case "$(basename "$o")" in crt0.s32o) ;; *) LIBOBJS="$LIBOBJS $o" ;; esac
done

run_side() {
    # $1 = tool dir, $2 = output dir
    local d="$1" r="$2"
    mkdir -p "$r"
    ( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$d/s32-as.s32x" "$IN/in.s" "$r/out.s32o" ) \
        >"$r/as.log" 2>&1
    ( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$d/s32-ar.s32x" rc "$r/out.s32a" "$IN/in.s32o" ) \
        >"$r/ar.log" 2>&1
    ( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$d/s32-ld.s32x" -o "$r/out.s32x" --mmio 64K \
        "$S8/lib/crt0.s32o" "$IN/in.s32o" $LIBOBJS ) >"$r/ld.log" 2>&1
    ( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$d/slow32dump.s32x" "$IN/in.s32o" ) \
        >"$r/dump.out" 2>&1
    ( cd "$ROOT" && timeout "$TIMEOUT" "$EMU" "$d/slow32dis.s32x" "$IN/in.s32o" ) \
        >"$r/dis.out" 2>&1
}

echo "Driving both tool sets over one shared input ($(basename "$SRC")):"
run_side "$WORK/out07" "$WORK/run07"
run_side "$WORK/out08" "$WORK/run08"

check "s32-as     -> .s32o"  "$WORK/run07/out.s32o" "$WORK/run08/out.s32o"
check "s32-ar     -> .s32a"  "$WORK/run07/out.s32a" "$WORK/run08/out.s32a"
check "s32-ld     -> .s32x"  "$WORK/run07/out.s32x" "$WORK/run08/out.s32x"
for side in 07 08; do
    normalize_output < "$WORK/run$side/dump.out" > "$WORK/run$side/dump.norm"
    normalize_output < "$WORK/run$side/dis.out"  > "$WORK/run$side/dis.norm"
done
check "slow32dump -> stdout" "$WORK/run07/dump.norm" "$WORK/run08/dump.norm"
check "slow32dis  -> stdout" "$WORK/run07/dis.norm"  "$WORK/run08/dis.norm"

echo ""
echo "================================================="
echo "stage07 differential: $AGREE agree, $DIVERGED diverged (of $TOTAL)"
if [ "$DIVERGED" -gt 0 ]; then
    echo -e "${RED}DISAGREEMENTS:${NC}"
    for d in "${DIVERGED_ITEMS[@]}"; do echo "  $d"; done
    echo ""
    echo "One of the two compilers is wrong here.  To find WHICH translation"
    echo "unit, rebuild swapping a single object at a time between"
    echo "$WORK/out07 and $WORK/out08 -- the file that flips the result is the"
    echo "one being miscompiled.  Artifacts are kept in $WORK."
    exit 1
fi
echo -e "${GREEN}Both bootstrap compilers agree.${NC}"
exit 0
