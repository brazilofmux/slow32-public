#!/bin/bash
# SLOW-32 xBase built entirely by stage08 cc, the self-hosted compiler,
# held to what the clang build does:
#
#   1. the interpreter (xbase/src, main.c included) compiled by stage08:
#      xbase/tests/run-tests.sh must pass on it;
#   2. the runtime library (xbase/src minus main.c, plus cliprt) and every
#      program's generated C compiled by stage08: the xbc oracle, whose
#      oracle stays the clang-built interpreter, must agree.
#
# About 30,000 lines stage08 sees nowhere else, and C compiled by both
# compilers meeting at every call (selfhost ISSUES-102 came from here):
# run it after a stage08 change, beside sqlite/check-stage08.sh.
#
#   S32_CC_S32X   stage08's cc.s32x (default selfhost/stage08/cc.s32x)
#   S32_CC_EMU    the engine it runs on (default tools/dbt/slow32-dbt)
set -u
CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
CC_S32X="${S32_CC_S32X:-$ROOT/selfhost/stage08/cc.s32x}"
CC_EMU="${S32_CC_EMU:-$ROOT/tools/dbt/slow32-dbt}"
OUT="$CLIP_DIR/out/stage08"
mkdir -p "$OUT"
W=$(mktemp -d "$OUT/build.XXXXXX") || exit 2
trap 'rm -rf "${W:?}"' EXIT
export S32_CC_S32X="$CC_S32X" S32_CC_EMU="$CC_EMU"
fail=0

echo "=== the interpreter, by stage08"
objs=()
for src in "$ROOT"/xbase/src/*.c; do
    b="$(basename "$src" .c)"
    if ! "$CC_EMU" "$CC_S32X" -I"$ROOT/runtime/include" -DNDEBUG "$src" "$W/$b.s" >/dev/null 2>"$W/$b.log" ||
       [ ! -s "$W/$b.s" ]; then
        echo "compile failed: $b.c"; grep -m3 error "$W/$b.log"; exit 1
    fi
    "$ROOT/tools/assembler/slow32asm" "$W/$b.s" "$W/$b.s32o" >/dev/null || { echo "assemble failed: $b"; exit 1; }
    objs+=("$W/$b.s32o")
done
"$ROOT/tools/linker/s32-ld" --mmio 64K --stack-size 128K --data-size 8M --heap-size 64M \
    -o "$OUT/xbase.s32x" "$ROOT/runtime/crt0.s32o" "${objs[@]}" \
    "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" || { echo "link failed"; exit 1; }
XBASE="$OUT/xbase.s32x" bash "$ROOT/xbase/tests/run-tests.sh" > "$OUT/suite.log" 2>&1 || fail=1
grep -E "FAIL|passed" "$OUT/suite.log"

echo "=== the runtime library and the programs, by stage08"
XBC_CC=stage08 bash "$CLIP_DIR/build-lib.sh" "$OUT/libxbase.s32a" || exit 1
XBC_CC=stage08 XBASE_LIB="$OUT/libxbase.s32a" bash "$CLIP_DIR/tests/run-oracle.sh" > "$OUT/oracle.log" 2>&1 || fail=1
grep -vE "AGREE|SKIP" "$OUT/oracle.log"

[ "$fail" -eq 0 ] && echo "xbase by stage08: PASS" || echo "xbase by stage08: FAIL"
exit "$fail"
