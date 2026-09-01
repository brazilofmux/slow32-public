#!/bin/bash

# SLOW-32 differential harness for the KIT TOOLS THEMSELVES
#
# run-kit-differential.sh runs 44 small stage08 test programs, 16 of which
# print nothing. This one drives the largest self-hosted binaries that exist --
# cc.s32x, s32-as.s32x, s32-ld.s32x, s32-ar.s32x, slow32dis.s32x,
# slow32dump.s32x -- over real inputs, under every engine, and byte-compares
# the artifacts each engine produced against the reference interpreter's.
#
# The inputs are the tools' OWN sources, so a clean run is a self-reproduction
# chain: the kit compiles, assembles and links its own assembler, and every
# engine must produce the identical object and executable. Any engine bug that
# perturbs a single emitted byte shows up as a hard mismatch rather than as a
# program that happens to print the same thing.
#
# QEMU is NOT in the roster: -kernel provides no guest argv, and every tool
# here needs arguments. That is a stated limitation, not an oversight.
#
# WHAT THIS DOES AND DOES NOT CATCH -- it does NOT supersede
# run-kit-differential.sh, and the two are not ranked. Measured by mutation on
# x86-64, 2026-09-01:
#
#   * Reintroducing DBT-15 (the "bge zero, rX" inversion): this harness
#     reported 21/21 AGREE -- it MISSED it entirely. run-kit-differential.sh
#     caught it with 4 divergences. Bigger binaries did not mean better
#     pattern coverage: the three sources here simply never exercise that
#     pattern in a way that reaches an artifact.
#   * An artificial SLTU fault (setb -> setbe): 12 of 21 checks diverged,
#     with byte counts, across all four produced artifacts.
#
# So this harness is the sharp detector for *artifact drift* -- any engine
# fault that perturbs an emitted byte anywhere in a 259KB compile is a hard
# mismatch -- while its instruction-pattern coverage is bounded by whatever
# these particular sources compile to. Run BOTH.
#
# Usage: ./run-kit-tools-differential.sh [source.c ...]
#   With no arguments, uses the default source list below.
#
# Env overrides: KIT (default ~/s32x), SLOW32, SLOW32_FAST, SLOW32_DBT,
#   TIMEOUT (secs, default 900), SOURCES (space-separated, repo-relative).
#
# Exit codes: 0 all engines agree, 1 divergence found, 2 setup error.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SLOW32="${SLOW32:-$ROOT/tools/emulator/slow32}"
SLOW32_FAST="${SLOW32_FAST:-$ROOT/tools/emulator/slow32-fast}"
SLOW32_DBT="${SLOW32_DBT:-$ROOT/tools/dbt/slow32-dbt}"

KIT="${KIT:-$HOME/s32x}"
SH="$KIT/selfhost"
WORK="$SCRIPT_DIR/results-kit-tools"
TIMEOUT="${TIMEOUT:-900}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

DEFAULT_SOURCES="selfhost/src/tools/s32-ar.c
selfhost/src/tools/slow32dis.c
selfhost/src/tools/s32-as.c"

if [ ! -x "$SLOW32" ]; then
    echo "ERROR: reference interpreter not found: $SLOW32" >&2
    exit 2
fi
for f in "$KIT/cc.s32x" "$KIT/s32-as.s32x" "$KIT/s32-ld.s32x" "$KIT/s32-ar.s32x" \
         "$KIT/slow32dis.s32x" "$KIT/slow32dump.s32x" \
         "$SH/crt0.s32o" "$SH/libc.s32a"; do
    if [ ! -e "$f" ]; then
        echo "ERROR: kit incomplete, missing: $f" >&2
        echo "       set KIT=<dir> (default ~/s32x)" >&2
        exit 2
    fi
done

# Engine roster: name|path. The reference is separate and always first.
ENGINES=()
SKIPPED_ENGINES=()
for spec in "slow32-fast|$SLOW32_FAST" "slow32-dbt|$SLOW32_DBT"; do
    path="${spec#*|}"
    if [ -x "$path" ]; then
        ENGINES+=("$spec")
    else
        SKIPPED_ENGINES+=("${spec%%|*} ($path)")
    fi
done
if [ ${#ENGINES[@]} -eq 0 ]; then
    echo "ERROR: no engines to compare against the reference" >&2
    exit 2
fi

echo "Kit:       $KIT"
echo "Reference: $SLOW32"
echo "Engines under test:"
for spec in "${ENGINES[@]}"; do echo "  ${spec%%|*}: ${spec#*|}"; done
for s in "${SKIPPED_ENGINES[@]}"; do
    echo -e "  ${YELLOW}SKIPPED ENGINE${NC}: $s -- not built/available on this host"
done
echo -e "  ${YELLOW}NOT APPLICABLE${NC}: qemu -- -kernel passes no guest argv; every tool here needs args"
echo ""

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
        -e '/^  strlen:  *0x[0-9a-f]*$/d' \
    | awk '{ lines[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR }
           END { for (i = 1; i <= last; i++) print lines[i] }'
}

TOTAL=0
AGREE=0
DIVERGED=0
DIVERGED_ITEMS=()

# run_tool <engine-path> <outdir> <stdout-file> <tool.s32x> [args...]
# The kit tools exit 96 on success under some paths (see build-tools.sh), so
# both 0 and 96 count as clean.
run_tool() {
    local engine="$1" out="$2"; shift 2
    local rc=0
    ( cd "$ROOT" && timeout "$TIMEOUT" "$engine" "$@" ) >"$out" 2>&1 || rc=$?
    echo "$rc"
}

ok_rc() { [ "$1" = "0" ] || [ "$1" = "96" ]; }

# Compare one produced artifact (or stdout) across engines.
# check <label> <ref-file> <engine-name:file> ...
check() {
    local label="$1"; shift
    local ref="$1"; shift
    TOTAL=$((TOTAL + 1))
    printf "  %-42s " "$label"
    local bad=()
    local pair name file
    for pair in "$@"; do
        name="${pair%%:*}"; file="${pair#*:}"
        if [ ! -f "$file" ]; then
            bad+=("$name(missing)")
        elif ! cmp -s "$ref" "$file"; then
            bad+=("$name($(cmp -l "$ref" "$file" 2>/dev/null | wc -l | tr -d ' ') bytes differ)")
        fi
    done
    if [ ${#bad[@]} -eq 0 ]; then
        echo -e "${GREEN}AGREE${NC}"
        AGREE=$((AGREE + 1))
    else
        echo -e "${RED}DIVERGE${NC}: ${bad[*]}"
        DIVERGED=$((DIVERGED + 1))
        DIVERGED_ITEMS+=("$label: ${bad[*]}")
    fi
}

do_source() {
    local src="$1"
    local base
    base="$(basename "$src" .c)"
    echo "=== $src"

    if [ ! -f "$ROOT/$src" ]; then
        echo -e "  ${YELLOW}SKIP${NC} (no such source)"
        return
    fi

    # Reference first, then each engine, each into its own directory.
    local all=("ref|$SLOW32")
    local spec
    for spec in "${ENGINES[@]}"; do all+=("$spec"); done

    local name path dir rc
    for spec in "${all[@]}"; do
        name="${spec%%|*}"; path="${spec#*|}"
        dir="$WORK/$base/$name"
        rm -rf "$dir"; mkdir -p "$dir"

        rc=$(run_tool "$path" "$dir/cc.log" "$KIT/cc.s32x" "$ROOT/$src" "$dir/out.s")
        ok_rc "$rc" || echo "    ($name: cc rc=$rc)"
        rc=$(run_tool "$path" "$dir/as.log" "$KIT/s32-as.s32x" "$dir/out.s" "$dir/out.s32o")
        ok_rc "$rc" || echo "    ($name: as rc=$rc)"
        rc=$(run_tool "$path" "$dir/ld.log" "$KIT/s32-ld.s32x" -o "$dir/out.s32x" \
                --mmio 64K "$SH/crt0.s32o" "$dir/out.s32o" "$SH/libc.s32a")
        ok_rc "$rc" || echo "    ($name: ld rc=$rc)"
        rc=$(run_tool "$path" "$dir/ar.log" "$KIT/s32-ar.s32x" rc "$dir/out.s32a" "$dir/out.s32o")
        ok_rc "$rc" || echo "    ($name: ar rc=$rc)"

        # Readers all run over the REFERENCE's artifacts, so a divergence here
        # is the reader engine's fault and not an inherited bad input.
        run_tool "$path" "$dir/dis.out" "$KIT/slow32dis.s32x" "$WORK/$base/ref/out.s32x" >/dev/null
        normalize_output < "$dir/dis.out" > "$dir/dis.norm"
        run_tool "$path" "$dir/dump.out" "$KIT/slow32dump.s32x" "$WORK/$base/ref/out.s32o" >/dev/null
        normalize_output < "$dir/dump.out" > "$dir/dump.norm"
        run_tool "$path" "$dir/art.out" "$KIT/s32-ar.s32x" t "$WORK/$base/ref/out.s32a" >/dev/null
        normalize_output < "$dir/art.out" > "$dir/art.norm"
    done

    local R="$WORK/$base/ref"
    local a_s=() a_o=() a_x=() a_a=() a_dis=() a_dump=() a_art=()
    for spec in "${ENGINES[@]}"; do
        name="${spec%%|*}"; dir="$WORK/$base/$name"
        a_s+=("$name:$dir/out.s");        a_o+=("$name:$dir/out.s32o")
        a_x+=("$name:$dir/out.s32x");     a_a+=("$name:$dir/out.s32a")
        a_dis+=("$name:$dir/dis.norm");   a_dump+=("$name:$dir/dump.norm")
        a_art+=("$name:$dir/art.norm")
    done

    check "cc.s32x    -> .s"        "$R/out.s"      "${a_s[@]}"
    check "s32-as     -> .s32o"     "$R/out.s32o"   "${a_o[@]}"
    check "s32-ld     -> .s32x"     "$R/out.s32x"   "${a_x[@]}"
    check "s32-ar rc  -> .s32a"     "$R/out.s32a"   "${a_a[@]}"
    check "slow32dis  -> stdout"    "$R/dis.norm"   "${a_dis[@]}"
    check "slow32dump -> stdout"    "$R/dump.norm"  "${a_dump[@]}"
    check "s32-ar t   -> stdout"    "$R/art.norm"   "${a_art[@]}"
    echo ""
}

mkdir -p "$WORK"

if [ $# -gt 0 ]; then
    for s in "$@"; do do_source "$s"; done
else
    printf '%s\n' "${SOURCES:-$DEFAULT_SOURCES}" | while read -r s; do
        [ -n "$s" ] || continue
        echo "$s"
    done > "$WORK/.srclist"
    while read -r s; do
        [ -n "$s" ] || continue
        do_source "$s"
    done < "$WORK/.srclist"
fi

echo "================================================="
echo "Kit-tools differential: $AGREE agree, $DIVERGED diverged (of $TOTAL checks)"
if [ ${#SKIPPED_ENGINES[@]} -gt 0 ]; then
    names=""
    for s in "${SKIPPED_ENGINES[@]}"; do names="$names ${s%% *}"; done
    echo -e "${YELLOW}NOTE${NC}: engines not compared this run:$names"
fi
if [ "$DIVERGED" -gt 0 ]; then
    echo -e "${RED}DIVERGENCES:${NC}"
    for d in "${DIVERGED_ITEMS[@]}"; do echo "  $d"; done
    exit 1
fi
echo -e "${GREEN}All engines agree.${NC}"
exit 0
