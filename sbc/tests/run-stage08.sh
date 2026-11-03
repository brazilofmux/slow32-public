#!/bin/bash
# SLOW BASIC built by stage08 cc, the self-hosted compiler: the
# interpreter (every sbasic/src file, held to sbasic/tests), and the
# compiled programs --
# as the slow32:basic image builds them: s32-sbc's C through cc.s32x on
# the emulator, linked with the interpreter's runtime as one archive
# (build-lib.sh) -- held to the interpreter by the oracle
# (run-oracle.py), whose oracle stays the clang-built sbasic.s32x.
#
# The C s32-sbc writes is unlike anything else stage08 compiles: a
# dispatch over hundreds of ops, chunked into functions of ~32KB of C,
# with the runtime's inline helpers in every one.  It found two fixed
# ceilings (the dominance frontier's 64 a block, 16384 blocks); run it
# after a stage08 change, beside sqlite/check-stage08.sh and
# xbc/tests/run-stage08.sh.
#
#   sbc/tests/run-stage08.sh [run-oracle.py arguments]
#
#   S32_CC_S32X   stage08's cc.s32x (default selfhost/stage08/cc.s32x)
#   S32_CC_EMU    the engine it runs on (default tools/dbt/slow32-dbt)
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SBC_DIR="$(dirname "$HERE")"
ROOT="$(dirname "$SBC_DIR")"
export S32_CC_S32X="${S32_CC_S32X:-$ROOT/selfhost/stage08/cc.s32x}"
export S32_CC_EMU="${S32_CC_EMU:-$ROOT/tools/dbt/slow32-dbt}"
mkdir -p "$SBC_DIR/out"
W=$(mktemp -d "$SBC_DIR/out/stage08.XXXXXX") || exit 2
trap 'rm -rf "${W:?}"' EXIT
fail=0

# 1. The interpreter itself, every source compiled by stage08 and linked
# with the clang-built runtime, held to its own suite.  Two of its files
# did not compile until a block-scope static list with brace
# initializers and a T (**p)[N] parameter were taken (2026-11-03).
echo "=== the interpreter, by stage08"
objs=()
for src in "$ROOT"/sbasic/src/*.c; do
    b="$(basename "$src" .c)"
    if ! "$S32_CC_EMU" "$S32_CC_S32X" -I"$ROOT/runtime/include" -DNDEBUG "$src" "$W/$b.s" >/dev/null 2>"$W/$b.log" ||
       [ ! -s "$W/$b.s" ]; then
        echo "compile failed: $b.c"; grep -m3 error "$W/$b.log"; exit 1
    fi
    "$ROOT/tools/assembler/slow32asm" "$W/$b.s" "$W/$b.s32o" >/dev/null || { echo "assemble failed: $b"; exit 1; }
    objs+=("$W/$b.s32o")
done
"$ROOT/tools/linker/s32-ld" --mmio 64K --stack-size 1M --heap-size 16M -o "$W/sbasic.s32x" \
    "$ROOT/runtime/crt0.s32o" "${objs[@]}" "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" ||
    { echo "link failed"; exit 1; }
SBASIC="$W/sbasic.s32x" bash "$ROOT/sbasic/tests/run-tests.sh" > "$W/suite.log" 2>&1 || fail=1
grep -E "FAIL|passed" "$W/suite.log"

# 2. Compiled programs: s32-sbc's C by stage08, held to the interpreter.
echo "=== compiled programs, by stage08"
bash "$SBC_DIR/build-lib.sh" "$W/libsbasic.s32a" >/dev/null || { echo "build-lib.sh failed"; exit 1; }
echo "compiled by: $S32_CC_S32X (on $(basename "$S32_CC_EMU"))"
SBC_CC=stage08 SBASIC_LIB="$W/libsbasic.s32a" python3 "$HERE/run-oracle.py" "$@" || fail=1
exit "$fail"
