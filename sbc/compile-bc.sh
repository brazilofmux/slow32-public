#!/bin/bash
# compile-bc.sh prog.bas [prog.s32x] -- a BASIC program through s32-bc:
# its assembly, assembled, linked with bc_rt and the interpreter's own
# objects (sbasic/build.sh makes them; nothing of the interpreter's main),
# whose code computes and prints every value the way it does.
#
#   S32_BC       s32-bc (default: this directory's, rebuilt when stale)
#   BC_RT        bc_rt.s32o (default: out/'s, build-bc.sh's)
#   SBASIC_LIB   the interpreter's objects as one archive (build-lib.sh's
#                libsbasic.s32a, what an installed copy -- the slow32:basic
#                image -- links); unset: sbasic's own objects
#   S32_AS S32_LD the assembler and linker;  S32_RT  crt0.s32o and the libraries
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
BC="${S32_BC:-}"
if [ -z "$BC" ]; then
    bash "$SBC_DIR/build-bc.sh" >/dev/null
    BC="$SBC_DIR/s32-bc"
fi
BCRT="${BC_RT:-$SBC_DIR/out/bc_rt.s32o}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/bc.XXXXXX")"
trap 'rm -rf "${WORK:?}"' EXIT
"$BC" "$SRC" "$WORK/$BASE.s"
"$ASM" "$WORK/$BASE.s" "$WORK/$BASE.s32o" >/dev/null
OBJS=()
if [ -n "${SBASIC_LIB:-}" ]; then
    OBJS=("$SBASIC_LIB")
else
    for o in "$SB"/*.s32o; do
        [ "$(basename "$o")" = main.s32o ] || OBJS+=("$o")
    done
fi
"$LD" --mmio 64K --stack-size 1M --heap-size 16M -o "$OUTX" \
    "$RT/crt0.s32o" "$WORK/$BASE.s32o" "$BCRT" "${OBJS[@]}" \
    "$RT/libc_mmio.s32a" "$RT/libs32.s32a"
