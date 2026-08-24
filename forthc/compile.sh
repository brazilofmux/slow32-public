#!/bin/bash
# forthc pipeline: prog.fth -> prog.s (via forthc.fth on the DTC
# kernel) -> prog.s32o (slow32asm) -> prog.s32x (s32-ld, standalone).
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SCRIPT_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

SRC="$1"
OUT="${2:-${1%.fth}.s32x}"
[ -f "$SRC" ] || { echo "usage: compile.sh prog.fth [prog.s32x]"; exit 2; }
ASM="${OUT%.s32x}.s"
OBJ="${OUT%.s32x}.s32o"

printf 'S" %s" S" %s" FORTHC BYE\n' "$SRC" "$ASM" \
    | cat "$ROOT/forth/prelude.fth" "$SCRIPT_DIR/forthc.fth" - \
    | "$EMU" "$ROOT/forth/kernel.s32x" \
    | grep -E "^forthc:" || true
[ -s "$ASM" ] || { echo "compile.sh: forthc produced no assembly"; exit 1; }
"$ROOT/tools/assembler/slow32asm" "$ASM" "$OBJ" >/dev/null
"$ROOT/tools/linker/s32-ld" -o "$OUT" "$OBJ"
echo "compile.sh: $OUT"
