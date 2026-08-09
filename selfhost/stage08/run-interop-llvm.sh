#!/usr/bin/env bash
# Mixed-compiler link test: one .s32x from BOTH clang (LLVM slow32 backend)
# and selfhost stage08 s12cc, calling each other in both directions.
#
# Verified interoperable (2026-08-08, gates below): int/pointer args, >8 args
# on the stack, long long register pairs + r1:r2 return, s12cc->clang
# callbacks, s12cc code against clang's crt0/libc_debug, and integer varargs
# defined by s12cc and called by clang (call-site convention is shared; va_*
# machinery is callee-private).
#
# Known-divergent (probed, reported, NOT gated): double arguments (FP
# word-order/pairing differs) and struct-by-value (s12cc hidden-pointer ABI
# vs clang's in-register). Fixing either means picking one convention.
#
# Link recipe: the s12cc side needs selfhost/stage08/builtins64.s (__muldi3;
# clang inlines MUL via UMUL_LOHI) and builtins_fp64.s (__fp64_mul etc.;
# clang uses HW FP). Both coexist with runtime/libs32.s32a.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
RUN="${RUN:-$ROOT/tools/emulator/slow32}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
CC_S32X="$SCRIPT_DIR/cc.s32x"
W="$(mktemp -d /tmp/interop-llvm.XXXXXX)"
trap '[ "${KEEP:-0}" = 1 ] || rm -rf "$W"' EXIT

cat > "$W/lib_s12.c" <<'EOF'
extern int cb(int x);
int add3(int a, int b, int c) { return a + b + c; }
int sum9(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
    return a + b + c + d + e + f + g + h + i;
}
unsigned long long mul64(unsigned long long a, unsigned int b) { return a * b; }
int call_back_twice(int seed) { return cb(seed) + cb(seed + 1); }
typedef char *va_list;
int vsum(int n, ...) {
    va_list ap; int i; int s;
    __builtin_va_start(ap, n);
    s = 0;
    for (i = 0; i < n; i++) s = s + __builtin_va_arg(ap, int);
    __builtin_va_end(ap);
    return s;
}
double dmul(double a, double b) { return a * b; }
struct Pt { int x; int y; };
int ptsum(struct Pt p) { return p.x + p.y; }
EOF
cat > "$W/main_clang.c" <<'EOF'
#include <stdio.h>
extern int add3(int, int, int);
extern int sum9(int,int,int,int,int,int,int,int,int);
extern unsigned long long mul64(unsigned long long, unsigned int);
extern int call_back_twice(int);
extern int vsum(int, ...);
extern double dmul(double, double);
struct Pt { int x; int y; };
extern int ptsum(struct Pt);
int cb(int x) { return x * 10; }
int main(void) {
    int fail = 0;
    if (add3(1,2,3) != 6) { fail = 1; printf("GATE add3 FAIL\n"); }
    if (sum9(1,2,3,4,5,6,7,8,9) != 45) { fail = 1; printf("GATE sum9 FAIL\n"); }
    if (mul64(0x100000001ULL, 7u) != 0x700000007ULL) { fail = 1; printf("GATE mul64 FAIL\n"); }
    if (call_back_twice(4) != 90) { fail = 1; printf("GATE callback FAIL\n"); }
    if (vsum(3, 10, 20, 30) != 60) { fail = 1; printf("GATE varargs FAIL\n"); }
    double d = dmul(2.5, 4.0);
    struct Pt p; p.x = 30; p.y = 12;
    printf("known-diverge double: %s\n", d == 10.0 ? "now PASSES (update script!)" : "still diverges");
    printf("known-diverge struct: %s\n", ptsum(p) == 42 ? "now PASSES (update script!)" : "still diverges");
    if (!fail) printf("INTEROP OK\n");
    return fail;
}
EOF
"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O2 -I"$ROOT/runtime/include" "$W/main_clang.c" -o "$W/main.ll"
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/main.ll" -o "$W/main.s"
"$AS" "$W/main.s" "$W/main.s32o" >/dev/null
"$EMU" "$CC_S32X" "$W/lib_s12.c" "$W/lib_s12.s" >/dev/null 2>&1
[ -s "$W/lib_s12.s" ] || { echo "s12cc produced no output"; exit 1; }
"$AS" "$W/lib_s12.s" "$W/lib_s12.s32o" >/dev/null
"$AS" "$SCRIPT_DIR/builtins64.s" "$W/b64.s32o" >/dev/null
"$AS" "$SCRIPT_DIR/builtins_fp64.s" "$W/bfp64.s32o" >/dev/null
"$LD" -o "$W/interop.s32x" "$ROOT/runtime/crt0.s32o" "$W/main.s32o" "$W/lib_s12.s32o" \
      "$W/b64.s32o" "$W/bfp64.s32o" "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null
out="$("$RUN" "$W/interop.s32x" 2>&1)"
echo "$out" | grep -E "GATE|diverge|INTEROP"
echo "$out" | grep -q "INTEROP OK" && echo "PASS: LLVM<->stage08 scalar interop" || { echo "FAIL"; exit 1; }
