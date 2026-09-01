#!/bin/bash

# SLOW-32 N-way differential harness for KIT-BUILT binaries
#
# run-differential.sh covers only *clang*-built test binaries. The stage08
# self-hosted compiler emits instruction patterns clang never emits, so its
# output was never differentially tested. That gap hid DBT-15 (x86-64
# translated "bge zero, rX" backwards): the clang suite passed 82/82 on the
# affected host while every kit-built printf silently dropped its conversions.
#
# This harness builds the stage08 test corpus with the KIT toolchain
# (cc.s32x -> s32-as.s32x -> s32-ld.s32x, all running under an emulator) and
# then runs each resulting binary under every available engine, diffing
# against the reference interpreter.
#
# Usage: ./run-kit-differential.sh [test-name ...]
#   With no arguments, builds and runs the whole corpus.
#   REBUILD=1 forces a rebuild of binaries already in results-kit/.
#
# Env overrides: KIT (default ~/s32x), SRC_DIR, SLOW32, SLOW32_FAST,
#   SLOW32_DBT, QEMU_S32, BUILD_ENGINE, TIMEOUT (secs).
#
# Exit codes: 0 all engines agree, 1 divergence found, 2 setup error.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"

SLOW32="${SLOW32:-$SLOW32_BASE/tools/emulator/slow32}"
SLOW32_FAST="${SLOW32_FAST:-$SLOW32_BASE/tools/emulator/slow32-fast}"
SLOW32_DBT="${SLOW32_DBT:-$SLOW32_BASE/tools/dbt/slow32-dbt}"
QEMU_S32="${QEMU_S32:-$HOME/qemu/build/qemu-system-slow32}"

KIT="${KIT:-$HOME/s32x}"
SH="$KIT/selfhost"
SRC_DIR="${SRC_DIR:-$SLOW32_BASE/selfhost/stage08/tests}"
CORPUS_INC="${CORPUS_INC:-$(dirname "$SRC_DIR")}"
RESULTS_DIR="$SCRIPT_DIR/results-kit"
TIMEOUT="${TIMEOUT:-20}"
BUILD_TIMEOUT="${BUILD_TIMEOUT:-180}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

if [ ! -x "$SLOW32" ]; then
    echo "ERROR: reference interpreter not found: $SLOW32" >&2
    exit 2
fi
for f in "$KIT/cc.s32x" "$KIT/s32-as.s32x" "$KIT/s32-ld.s32x" \
         "$SH/crt0.s32o" "$SH/libc.s32a" "$SH/include"; do
    if [ ! -e "$f" ]; then
        echo "ERROR: kit incomplete, missing: $f" >&2
        echo "       set KIT=<dir> (default ~/s32x)" >&2
        exit 2
    fi
done
if [ ! -d "$SRC_DIR" ]; then
    echo "ERROR: source corpus not found: $SRC_DIR" >&2
    exit 2
fi

# The engine that DRIVES the build. Any engine can host the toolchain; the
# comparison below is about executing the built binary, not producing it.
# Prefer the faster interpreter so a 44-source corpus stays interactive.
if [ -n "$BUILD_ENGINE" ]; then
    BUILD="$BUILD_ENGINE"
elif [ -x "$SLOW32_FAST" ]; then
    BUILD="$SLOW32_FAST"
else
    BUILD="$SLOW32"
fi

# Engine roster: name|path|compare-exit-code
ENGINES=()
SKIPPED_ENGINES=()
for spec in "slow32-fast|$SLOW32_FAST|1" \
            "slow32-dbt|$SLOW32_DBT|1" \
            "qemu|$QEMU_S32|0"; do
    name="${spec%%|*}"
    path="$(echo "$spec" | cut -d'|' -f2)"
    if [ -x "$path" ]; then
        ENGINES+=("$spec")
    else
        SKIPPED_ENGINES+=("$name ($path)")
    fi
done

if [ ${#ENGINES[@]} -eq 0 ]; then
    echo "ERROR: no engines to compare against the reference" >&2
    exit 2
fi

echo "Kit:       $KIT"
echo "Sources:   $SRC_DIR"
echo "Built by:  $BUILD (driving cc.s32x / s32-as.s32x / s32-ld.s32x)"
echo "Reference: $SLOW32"
echo "Engines under test:"
for spec in "${ENGINES[@]}"; do
    echo "  ${spec%%|*}: $(echo "$spec" | cut -d'|' -f2)"
done
if [ ${#SKIPPED_ENGINES[@]} -gt 0 ]; then
    for s in "${SKIPPED_ENGINES[@]}"; do
        echo -e "  ${YELLOW}SKIPPED ENGINE${NC}: $s -- not built/available on this host"
    done
fi
echo ""

normalize_output() {
    sed \
        -e 's/\r$//' \
        -e 's/^Memory fault: Failed to .* bytes at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^Error: .* out of bounds at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^Error: .* out of bounds or to protected memory at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^DBT: Memory fault at PC=[0-9xA-Fa-f]*, addr=\(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
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
        -e '/^  strlen:  *0x[0-9a-f]*$/d' \
    | awk '
        /^FAULT addr=/ { $0 = tolower($0) }
        { lines[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR }
        END { for (i = 1; i <= last; i++) print lines[i] }
    '
}

TOTAL=0
AGREE=0
DIVERGED=0
SKIPPED=0
THIN=0
DIVERGED_TESTS=()

run_engine() {
    local name="$1" path="$2" s32x="$3" out="$4"
    shift 4
    local rc=0
    if [ "$name" = "qemu" ]; then
        timeout "$TIMEOUT" "$path" -machine slow32-tcg -nographic -monitor none \
            -kernel "$s32x" </dev/null >"$out" 2>&1 || rc=$?
    else
        timeout "$TIMEOUT" "$path" "$s32x" </dev/null >"$out" 2>&1 || rc=$?
    fi
    echo "$rc"
}

# Build one source with the kit. Returns 0 on success; build logs are kept
# next to the artifacts so a build failure can be told from a run failure.
#
# The pipeline runs in a SUBSHELL, not a brace group: a brace group shares the
# shell, so an "exit 1" on the first failing source would take the whole
# harness down with it (silently, mid-line).
#
# Sources are compiled with their own directory AND the corpus root on the
# include path as well as the kit's -- the corpus has files that pull in a
# generated source by relative name from one level up (lex_test.c does
# #include "c_lexer_gen.c", which lives in stage08/, not stage08/tests/).
build_kit() {
    local src="$1" dir="$2" base="$3"
    local log="$dir/build.log"
    local srcdir
    srcdir="$(cd "$(dirname "$src")" && pwd)"
    : > "$log"
    (
        set -e
        echo "=== cc"
        timeout "$BUILD_TIMEOUT" "$BUILD" "$KIT/cc.s32x" \
            -I"$SH/include" -I"$srcdir" -I"$CORPUS_INC" "$src" "$dir/$base.s"
        echo "=== as"
        timeout "$BUILD_TIMEOUT" "$BUILD" "$KIT/s32-as.s32x" \
            "$dir/$base.s" "$dir/$base.s32o"
        echo "=== ld"
        timeout "$BUILD_TIMEOUT" "$BUILD" "$KIT/s32-ld.s32x" \
            -o "$dir/test.s32x" --mmio 64K \
            "$SH/crt0.s32o" "$dir/$base.s32o" "$SH/libc.s32a"
    ) >>"$log" 2>&1
    [ -f "$dir/test.s32x" ]
}

run_test() {
    local src="$1"
    local base
    base="$(basename "$src" .c)"
    local dir="$RESULTS_DIR/$base"
    local s32x="$dir/test.s32x"
    local diff_dir="$dir/diff"

    TOTAL=$((TOTAL + 1))
    printf "%-34s " "$base:"

    mkdir -p "$dir"
    if [ ! -f "$s32x" ] || [ -n "$REBUILD" ]; then
        rm -f "$s32x"
        if ! build_kit "$src" "$dir" "$base"; then
            echo -e "${YELLOW}SKIP${NC} (kit build failed; see $dir/build.log)"
            SKIPPED=$((SKIPPED + 1))
            return
        fi
    fi

    mkdir -p "$diff_dir"

    local ref_rc
    ref_rc=$(run_engine ref "$SLOW32" "$s32x" "$diff_dir/ref.out")
    normalize_output < "$diff_dir/ref.out" > "$diff_dir/ref.norm"

    # A binary that faults or times out under the REFERENCE is not a usable
    # differential subject -- the corpus has sources that need argv or exercise
    # unsupported shapes. Say so rather than comparing garbage.
    if [ "$ref_rc" -eq 124 ]; then
        echo -e "${YELLOW}SKIP${NC} (reference timed out)"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    # A test that prints nothing agrees on output vacuously -- the only real
    # evidence left is its exit code. Count those separately so the summary
    # does not read as more coverage than it is.
    local thin=0
    if [ ! -s "$diff_dir/ref.norm" ]; then
        thin=1
        THIN=$((THIN + 1))
    fi

    local bad=()
    local spec name path cmp_rc rc
    for spec in "${ENGINES[@]}"; do
        name="${spec%%|*}"
        path="$(echo "$spec" | cut -d'|' -f2)"
        cmp_rc="$(echo "$spec" | cut -d'|' -f3)"
        rc=$(run_engine "$name" "$path" "$s32x" "$diff_dir/$name.out")
        normalize_output < "$diff_dir/$name.out" > "$diff_dir/$name.norm"
        if ! diff -q "$diff_dir/ref.norm" "$diff_dir/$name.norm" >/dev/null 2>&1; then
            bad+=("$name(output)")
        elif [ "$cmp_rc" = "1" ] && [ "$rc" != "$ref_rc" ]; then
            bad+=("$name(exit $rc vs $ref_rc)")
        fi
    done

    if [ ${#bad[@]} -eq 0 ]; then
        if [ "$thin" = "1" ]; then
            echo -e "${GREEN}AGREE${NC} ${YELLOW}(exit code only; no output)${NC}"
        else
            echo -e "${GREEN}AGREE${NC}"
        fi
        AGREE=$((AGREE + 1))
    else
        echo -e "${RED}DIVERGE${NC}: ${bad[*]}"
        DIVERGED=$((DIVERGED + 1))
        DIVERGED_TESTS+=("$base: ${bad[*]}")
    fi
}

mkdir -p "$RESULTS_DIR"

if [ $# -gt 0 ]; then
    for t in "$@"; do
        src="$SRC_DIR/$t"
        [ -f "$src" ] || src="$SRC_DIR/$t.c"
        if [ ! -f "$src" ]; then
            echo "ERROR: no such source: $t" >&2
            exit 2
        fi
        run_test "$src"
    done
else
    for src in "$SRC_DIR"/*.c; do
        [ -f "$src" ] || continue
        run_test "$src"
    done
fi

echo ""
echo "================================================="
echo "Kit differential: $AGREE agree, $DIVERGED diverged, $SKIPPED skipped (of $TOTAL)"
if [ "$THIN" -gt 0 ]; then
    echo -e "${YELLOW}NOTE${NC}: $THIN of the $AGREE agreeing tests print nothing --"
    echo "      those agree on exit code alone, which is thin evidence."
fi
if [ ${#SKIPPED_ENGINES[@]} -gt 0 ]; then
    names=""
    for s in "${SKIPPED_ENGINES[@]}"; do names="$names ${s%% *}"; done
    echo -e "${YELLOW}NOTE${NC}: engines not compared this run:$names"
fi
if [ "$DIVERGED" -gt 0 ]; then
    echo -e "${RED}DIVERGENCES:${NC}"
    for d in "${DIVERGED_TESTS[@]}"; do echo "  $d"; done
    exit 1
fi
echo -e "${GREEN}All engines agree.${NC}"
exit 0
