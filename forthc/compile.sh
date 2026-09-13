#!/bin/bash
# forthc pipeline: prog.fth -> prog.s (via forthc.fth on the DTC
# kernel) -> prog.s32o (slow32asm) -> prog.s32x.
#
# Default: standalone (own _start, debug-only I/O, links alone).
# --hosted: emits `main` under crt0 + libc_mmio (+ --mmio 64K) so the
# compiled program can call the C runtime — tube words, MS, files.
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SCRIPT_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
# Tree paths by default; the slow32:forth image sets these to /opt/slow32
# (the same knobs the two test harnesses honour).
KERNEL="${S32_FORTH_KERNEL:-$ROOT/forth/kernel.s32x}"
PRELUDE="${S32_FORTH_PRELUDE:-$ROOT/forth/prelude.fth}"
AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"

HOSTED=0
if [ "${1:-}" = "--hosted" ]; then HOSTED=1; shift; fi

SRC="$1"
OUT="${2:-${1%.fth}.s32x}"
[ -f "$SRC" ] || { echo "usage: compile.sh [--hosted] prog.fth [prog.s32x]"; exit 2; }
ASM="${OUT%.s32x}.s"
OBJ="${OUT%.s32x}.s32o"

# prelude-fc provides the closed-world prelude vocabulary
FULLSRC="${OUT%.s32x}.full.fth"
cat "$SCRIPT_DIR/prelude-fc.fth" "$SRC" > "$FULLSRC"

MODE=""
[ "$HOSTED" = 1 ] && MODE="HOSTED "
printf '%sS" %s" S" %s" FORTHC BYE\n' "$MODE" "$FULLSRC" "$ASM" \
    | cat "$PRELUDE" "$SCRIPT_DIR/forthc.fth" - \
    | "$EMU" "$KERNEL" \
    | grep -E "^forthc:" || true
[ -s "$ASM" ] || { echo "compile.sh: forthc produced no assembly"; exit 1; }
"$AS" "$ASM" "$OBJ" >/dev/null
if [ "$HOSTED" = 1 ]; then
    "$LD" --mmio 64K -o "$OUT" \
        "$RT/crt0.s32o" "$OBJ" \
        "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
else
    "$LD" -o "$OUT" "$OBJ"
fi
echo "compile.sh: $OUT"
