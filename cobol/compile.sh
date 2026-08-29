#!/bin/bash
# compile.sh -- COBOL source to a SLOW-32 executable.
#   ./compile.sh [-free|-fixed] prog.cbl [-o prog.s32x]
# s32-cobc emits assembler; slow32asm and s32-ld do the rest, with libcob
# and the SLOW-32 libc.  Run the result with tools/emulator/slow32 or
# slow32-dbt.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
fmt="-fixed"; src=""; out=""
while [ $# -gt 0 ]; do
    case "$1" in
        -free|-fixed) fmt="$1" ;;
        -o) out="$2"; shift ;;
        *) src="$1" ;;
    esac
    shift
done
[ -n "$src" ] || { echo "usage: compile.sh [-free|-fixed] prog.cbl [-o prog.s32x]" >&2; exit 2; }
[ -n "$out" ] || out="${src%.cbl}.s32x"
[ -x "$HERE/out/s32-cobc" ] || "$HERE/build.sh" >/dev/null
base="${out%.s32x}"
"$HERE/out/s32-cobc" $fmt -o "$base.s" "$src"
"$ROOT/tools/assembler/slow32asm" "$base.s" "$base.s32o" >/dev/null
# The MMIO libc: files (fopen and friends) live only there, and the
# linker's --mmio gives the emulator the ring buffers to serve them.
"$ROOT/tools/linker/s32-ld" --mmio 64K --stack-size 128K --heap-size 8M -o "$out" "$ROOT/runtime/crt0.s32o" "$base.s32o" \
    "$HERE/libcob/libcob.s32o" "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null
echo "$out"
