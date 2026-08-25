#!/usr/bin/env bash
# FP differential gate: the same double/float torture compiled by BOTH
# stage08 cc (HW FP inline emission) and the LLVM slow32 backend must
# print bit-identical IEEE results — arithmetic, compares, negation,
# every conversion direction, extreme magnitudes (1e±300), and global
# double-array initializers (which exercise the lexer's decimal→binary
# conversion and the parser's FP init emission, not just codegen).
#
# History: this harness surfaced (1) double literals parsed at f32
# precision (lexer's old 24-bit converter), (2) global double-array
# initializers emitted as truncated ints, and (3) stage07/stage08
# allocating 4-byte slots for zero-init double scalar globals.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
RUN="${RUN:-$ROOT/tools/emulator/slow32}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
CC_S32X="$SCRIPT_DIR/cc.s32x"
W="$(mktemp -d /tmp/fp-diff.XXXXXX)"
trap '[ "${KEEP:-0}" = 1 ] || rm -rf "$W"' EXIT

cat > "$W/fptort.c" <<'EOF'
#include <stdio.h>
double vals[7] = {0.0, 1.5, -2.25, 3.14159265358979, 1e300, -1e-300, 123456789.125};
int main(void) {
    int i; int j;
    unsigned long long bits;
    double r;
    for (i = 0; i < 7; i++) {
        for (j = 0; j < 7; j++) {
            double a = vals[i]; double b = vals[j];
            r = a + b; bits = *(unsigned long long*)&r; printf("add %d %d %08x%08x\n", i, j, (unsigned)(bits>>32), (unsigned)bits);
            r = a - b; bits = *(unsigned long long*)&r; printf("sub %d %d %08x%08x\n", i, j, (unsigned)(bits>>32), (unsigned)bits);
            r = a * b; bits = *(unsigned long long*)&r; printf("mul %d %d %08x%08x\n", i, j, (unsigned)(bits>>32), (unsigned)bits);
            if (b != 0.0) { r = a / b; bits = *(unsigned long long*)&r; printf("div %d %d %08x%08x\n", i, j, (unsigned)(bits>>32), (unsigned)bits); }
            printf("cmp %d %d %d%d%d\n", i, j, a < b, a <= b, a == b);
        }
        r = -vals[i]; bits = *(unsigned long long*)&r; printf("neg %d %08x%08x\n", i, (unsigned)(bits>>32), (unsigned)bits);
        printf("d2i %d %d\n", i, (int)vals[i % 4]);
        printf("d2ll %d %lld\n", i, (long long)vals[i % 4]);
        r = (double)(i * 1000000 - 1500000); bits = *(unsigned long long*)&r; printf("i2d %d %08x%08x\n", i, (unsigned)(bits>>32), (unsigned)bits);
        r = (double)((long long)i * 123456789012345LL - 400000000000000LL); bits = *(unsigned long long*)&r; printf("ll2d %d %08x%08x\n", i, (unsigned)(bits>>32), (unsigned)bits);
        { float f = (float)vals[i]; printf("d2f %d %08x\n", i, *(unsigned*)&f); r = (double)f; bits = *(unsigned long long*)&r; printf("f2d %d %08x%08x\n", i, (unsigned)(bits>>32), (unsigned)bits); }
    }
    return 0;
}
EOF

# stage08 side (needs __muldi3 from builtins64; libs32 lacks it)
python3 - "$SCRIPT_DIR/builtins64.s" "$W/muldi3.s" <<'PYEOF'
import sys
src = open(sys.argv[1]).read().split("\n")
out = [".text"]; on = False
for ln in src:
    if ln.startswith(".global "):
        on = (ln.split()[1] == "__muldi3")
    if on: out.append(ln)
open(sys.argv[2], "w").write("\n".join(out) + "\n")
PYEOF
"$AS" "$W/muldi3.s" "$W/muldi3.s32o" >/dev/null
"$EMU" "$CC_S32X" -I"$ROOT/runtime/include" "$W/fptort.c" "$W/fptort_s08.s" >/dev/null 2>&1
[ -s "$W/fptort_s08.s" ] || { echo "stage08 cc produced no output"; exit 1; }
if grep -q "jal r31, __fp64_" "$W/fptort_s08.s"; then
    echo "FAIL: stage08 cc still emits __fp64_* calls (HW FP not inline)"
    exit 1
fi
"$AS" "$W/fptort_s08.s" "$W/fptort_s08.s32o" >/dev/null
"$LD" -o "$W/fptort_s08.s32x" "$ROOT/runtime/crt0.s32o" "$W/fptort_s08.s32o" \
      "$W/muldi3.s32o" "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null

# LLVM side
"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O2 -I"$ROOT/runtime/include" "$W/fptort.c" -o "$W/fptort.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/fptort.ll" -o "$W/fptort_cl.s"
"$AS" "$W/fptort_cl.s" "$W/fptort_cl.s32o" >/dev/null
"$LD" -o "$W/fptort_cl.s32x" "$ROOT/runtime/crt0.s32o" "$W/fptort_cl.s32o" \
      "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null

"$RUN" "$W/fptort_s08.s32x" 2>/dev/null | grep -v "^[A-Z]" > "$W/out_s08.txt"
"$RUN" "$W/fptort_cl.s32x" 2>/dev/null | grep -v "^[A-Z]" > "$W/out_cl.txt"
n="$(wc -l < "$W/out_cl.txt" | tr -d ' ')"
if [ "$n" -lt 200 ]; then echo "FAIL: reference produced only $n lines"; exit 1; fi
if diff -q "$W/out_s08.txt" "$W/out_cl.txt" >/dev/null; then
    echo "PASS: FP differential — $n results bit-identical (stage08 HW FP vs LLVM)"
else
    echo "FAIL: FP differential diverges:"
    diff "$W/out_s08.txt" "$W/out_cl.txt" | head -20
    exit 1
fi
