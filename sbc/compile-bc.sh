#!/bin/bash
# compile-bc.sh prog.bas [prog.s32x] -- a BASIC program through s32-bc:
# its assembly, assembled, linked with bc_rt and the interpreter's own
# objects (sbasic/build.sh makes them; nothing of the interpreter's main),
# whose code computes and prints every value the way it does.
set -euo pipefail
SBC_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SBC_DIR")"
ASM="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
SB="$ROOT/sbasic/src"
SRC="${1:?usage: compile-bc.sh prog.bas [prog.s32x]}"
BASE="$(basename "${SRC%.*}")"
OUTX="${2:-${SRC%.*}.s32x}"
bash "$SBC_DIR/build-bc.sh" >/dev/null
WORK="$(mktemp -d "${TMPDIR:-/tmp}/bc.XXXXXX")"
trap 'rm -rf "${WORK:?}"' EXIT
"$SBC_DIR/s32-bc" "$SRC" "$WORK/$BASE.s"
"$ASM" "$WORK/$BASE.s" "$WORK/$BASE.s32o" >/dev/null
OBJS=()
for o in "$SB"/*.s32o; do
    [ "$(basename "$o")" = main.s32o ] || OBJS+=("$o")
done
"$LD" --mmio 64K --stack-size 1M --heap-size 16M -o "$OUTX" \
    "$RT/crt0.s32o" "$WORK/$BASE.s32o" "$SBC_DIR/out/bc_rt.s32o" "${OBJS[@]}" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
