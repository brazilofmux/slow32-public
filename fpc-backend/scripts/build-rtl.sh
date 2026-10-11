#!/bin/bash
# Build the whole embedded RTL for SLOW-32 (sysutils, classes, math, ...)
# as the test suite and real programs need it, not just the system unit.
#
# The unit list and flags come from rtl/embedded's own Makefile (make -n),
# so a new unit there is picked up here.  Three adjustments:
#   - -s -n: assembly-only output (slow32asm assembles it), no fpc.cfg;
#   - the system unit is the in-tree one (fpc-backend/runtime/system.ppu,
#     built from rtl/slow32/system.pp, the fork that delegates the heap to
#     libc_mmio), not rtl/embedded/system.pp;
#   - everything runs from the output directory with absolute paths, and
#     the five unit sources that live in rtl/embedded are copied beside the
#     output.  Compiled from rtl/embedded, the compiler found that other
#     system.pp next to system.ppu, decided system needed recompiling and
#     died with Internal error 2026032615 in ctask.pas.
#
# Output (not in git): $OUT/*.ppu *.s *.s32o, system.s32o, fpcrtl.s32a (an
# archive of every unit but system, so a link pulls only what it uses).
#
# Usage: build-rtl.sh [OUT]     env: FPC_SRC (~/fpc), PPCS32
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKEND="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT="$(cd "$BACKEND/.." && pwd)"
FPC_SRC="${FPC_SRC:-$HOME/fpc}"
PPCS32="${PPCS32:-$FPC_SRC/compiler/ppcs32}"
OUT="${1:-$BACKEND/runtime/units}"
R="$FPC_SRC/rtl"
AS="$ROOT/tools/assembler/slow32asm"
AR="$ROOT/tools/utilities/s32-ar"
for f in "$PPCS32" "$R/embedded/Makefile" "$BACKEND/runtime/system.ppu" "$BACKEND/runtime/system.s" "$AS" "$AR"; do
    [[ -e "$f" ]] || { echo "missing: $f" >&2; exit 1; }
done
mkdir -p "$OUT"
rm -f "$OUT"/*.ppu "$OUT"/*.s "$OUT"/*.s32o "$OUT"/*.s32a "$OUT"/*.rsj
cp "$BACKEND/runtime/system.ppu" "$BACKEND/runtime/system.s" "$OUT/"
CMDS="$OUT/.build-commands.sh"
( cd "$R/embedded" && make -n CPU_TARGET=slow32 OS_TARGET=embedded FPC="$PPCS32" 2>/dev/null ) \
  | grep '^/.*ppcs32' | grep -v ' system.pp$' \
  | sed -E "s#-FE\. #-FE$OUT #; s#-FU[^ ]* #-FU$OUT -Fu$OUT #; s#ppcs32 #ppcs32 -s -n -Fi$R/embedded #; s#-Fi\.\./#-Fi$R/#g; s#-Fislow32#-Fi$R/embedded/slow32#; s# \.\./# $R/#g; s# ([a-z0-9_]+\.(pp|pas))\$# $OUT/\1#" > "$CMDS"
# the sources named bare in the Makefile live in rtl/embedded: copy them beside the output
for src in $(awk '{print $NF}' "$CMDS"); do
    b="$(basename "$src")"
    [[ -e "$R/embedded/$b" ]] && cp "$R/embedded/$b" "$OUT/$b"
done
ok=0; bad=0; LOG="$OUT/.build.log"; : > "$LOG"
cd "$OUT"
while read -r cmd; do
    u="$(basename "$(echo "$cmd" | awk '{print $NF}')")"
    if eval "$cmd" > "$OUT/.one.log" 2>&1; then ok=$((ok+1)); else
        bad=$((bad+1)); echo "FAILED: $u -- $(grep -m1 'Error\|Fatal' "$OUT/.one.log")"
    fi
    cat "$OUT/.one.log" >> "$LOG"
done < "$CMDS"
# Units the embedded Makefile does not list but the test suite and real
# programs want: the rtl-objpas package (variants, strutils, dateutils, rtti)
# and heaptrc.  Same flags; the package's include dir added.
OBJPAS="$FPC_SRC/packages/rtl-objpas/src/inc"
for src in "$OBJPAS/varutils.pp" "$OBJPAS/variants.pp" "$OBJPAS/strutils.pp" "$OBJPAS/dateutils.pp" "$OBJPAS/rtti.pp" "$R/inc/heaptrc.pp"; do
    u="$(basename "$src")"
    if "$PPCS32" -s -n -Tembedded -Pslow32 -Fi"$R/inc" -Fi"$R/slow32" -Fi"$R/embedded" -Fi"$R/embedded/slow32" -Fi"$OBJPAS" \
         -Fu"$OUT" -FE"$OUT" -FU"$OUT" "$src" > "$OUT/.one.log" 2>&1; then ok=$((ok+1)); else
        # ppcs32 dies with EBusError after writing varutils.ppu/.s (host-side, at exit); the outputs are whole
        if [[ -s "$OUT/${u%.pp}.ppu" && -s "$OUT/${u%.pp}.s" ]] && grep -q 'End asmlist al_dwarf_frame' "$OUT/${u%.pp}.s"; then ok=$((ok+1)); echo "note: $u compiled; the compiler crashed on its way out"; else
        bad=$((bad+1)); echo "FAILED: $u -- $(grep -m1 'Error\|Fatal' "$OUT/.one.log")"; fi
    fi
    cat "$OUT/.one.log" >> "$LOG"
done
echo "units compiled: $ok, failed: $bad (log: $LOG)"
n=0
for s in *.s; do
    "$AS" "$s" "${s%.s}.s32o" > "$OUT/.as.log" 2>&1 || { echo "ASSEMBLE FAILED: $s"; grep -m2 -i error "$OUT/.as.log"; exit 1; }
    n=$((n+1))
done
"$AR" c fpcrtl.s32a $(ls *.s32o | grep -v '^system\.s32o$') > "$OUT/.ar.log" 2>&1
echo "assembled $n objects; fpcrtl.s32a: $(wc -c < fpcrtl.s32a) bytes; system.s32o separate"
[[ $bad -eq 0 ]]
