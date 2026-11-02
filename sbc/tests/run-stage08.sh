#!/bin/bash
# The compiled programs built by stage08 cc, the self-hosted compiler --
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
bash "$SBC_DIR/build-lib.sh" "$W/libsbasic.s32a" >/dev/null || { echo "build-lib.sh failed"; exit 1; }
echo "compiled by: $S32_CC_S32X (on $(basename "$S32_CC_EMU"))"
SBC_CC=stage08 SBASIC_LIB="$W/libsbasic.s32a" python3 "$HERE/run-oracle.py" "$@"
