#!/bin/bash
# Build islisp three ways.
#
#   ./build.sh            host binary (out/islisp-host) and the SLOW-32
#                         executable built by clang (out/islisp.s32x)
#   ./build.sh host       the host binary only (fast iteration)
#   ./build.sh stage08    out/islisp-stage08.s32x through the kit toolchain
#                         (cc.s32x / s32-as / s32-ld under the emulator);
#                         the stage08 gate diffs its output against clang's
#
# islisp/ is in the tree's ordinary universe: the host compiler builds the
# interpreter, and LLVM builds it for SLOW-32 (docs/plans/islisp.md).
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="${S32_AS:-$ROOT/tools/assembler/slow32asm}"
LD="${S32_LD:-$ROOT/tools/linker/s32-ld}"
RT="${S32_RT:-$ROOT/runtime}"
KIT="${KIT:-$HOME/s32x}"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
OPT="${OPT:--O1}"
mkdir -p "$HERE/out"
SRCS="$HERE/src/heap.c $HERE/src/symbol.c $HERE/src/reader.c $HERE/src/printer.c $HERE/src/number.c $HERE/src/eval.c $HERE/src/cond.c $HERE/src/class.c $HERE/src/stream.c $HERE/src/builtin.c $HERE/src/seq.c $HERE/src/main.c"
what="${1:-all}"

build_host() {
    cc -std=c99 -O1 -g -Wall -Wno-unused-function -Wno-unused-label -o "$HERE/out/islisp-host" $SRCS -lm
    echo "built: $HERE/out/islisp-host"
}

build_clang() {
    local objs=""
    for src in $SRCS; do
        local b; b="$(basename "$src" .c)"
        "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT -nostdinc -fno-builtin \
            -I"$RT/include" "$src" -o "$HERE/out/$b.ll"
        "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$HERE/out/$b.ll" -o "$HERE/out/$b.s"
        "$AS" "$HERE/out/$b.s" "$HERE/out/$b.s32o" >/dev/null
        objs="$objs $HERE/out/$b.s32o"
    done
    "$LD" --mmio 64K --stack-size 4M --heap-size 32M -o "$HERE/out/islisp.s32x" \
        "$RT/crt0.s32o" $objs "$RT/libc_mmio.s32a" "$RT/libs32.s32a" >/dev/null
    echo "built: $HERE/out/islisp.s32x"
}

build_stage08() {
    # The kit's cc.s32x compiles C to SLOW-32 assembly under the emulator
    # (positional: cc -I dir src.c out.s, as sqlite/build-stage08.sh drives
    # it); the tree's assembler and linker finish the job against the
    # kit's crt0 and libc.
    local objs=""
    local dbt="$ROOT/tools/dbt/slow32-dbt"
    [ -x "$dbt" ] || dbt="$EMU"
    for src in $SRCS; do
        local b; b="$(basename "$src" .c)"
        "$dbt" "$KIT/cc.s32x" -I "$KIT/selfhost/include" "$src" "$HERE/out/$b.k.s" > "$HERE/out/$b.k.log" 2>&1 \
            || { tail -5 "$HERE/out/$b.k.log" >&2; echo "stage08 cc failed: $src" >&2; exit 1; }
        "$AS" "$HERE/out/$b.k.s" "$HERE/out/$b.k.s32o" >/dev/null
        objs="$objs $HERE/out/$b.k.s32o"
    done
    "$LD" --mmio 64K --stack-size 4M --heap-size 32M -o "$HERE/out/islisp-stage08.s32x" \
        "$KIT/selfhost/crt0.s32o" $objs "$KIT/selfhost/libc.s32a" >/dev/null
    echo "built: $HERE/out/islisp-stage08.s32x"
}

case "$what" in
    host) build_host ;;
    clang) build_clang ;;
    stage08) build_stage08 ;;
    all) build_host; build_clang ;;
    *) echo "build.sh: host | clang | stage08 | all" >&2; exit 2 ;;
esac
