#!/usr/bin/env bash
# Mixed-compiler link test: one .s32x from BOTH clang (LLVM slow32 backend)
# and selfhost stage08 s12cc, calling each other in both directions.
#
# Verified interoperable (gates below): int/pointer args, >8 args on the
# stack, long long register pairs + r1:r2 return, s12cc->clang callbacks,
# s12cc code against clang's crt0/libc_debug, integer varargs defined by
# s12cc and called by clang (call-site convention is shared; va_*
# machinery is callee-private), double arguments (clang's aligned-pair
# convention, closed 2026-08-24), and struct-by-value in BOTH directions
# (clang's byval convention, closed 2026-08-24: the caller copies the
# struct into its own frame and passes the copy's address in a stack
# slot that reserves the struct's full rounded size — never an argument
# register — with stack slots laid out in argument order; struct RETURN
# uses the shared hidden-pointer-in-r3 sret convention).
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
struct Pt mkpt(int x, int y) { struct Pt p; p.x = x; p.y = y; return p; }
struct Big { int a[6]; };
int bigmix(int a,int b,int c,int d,int e,int f,int g,int h,
           struct Big s, int i, int j) {
    return a + h + s.a[0] + s.a[5] + i + j;
}
/* Struct LAYOUT across the boundary: doubles and long longs align to
 * 4 in the SLOW-32 ABI, so this struct is 16 bytes with d at offset 4
 * and t at offset 12.  Passed by pointer, so only offsets are probed. */
struct Mix { char c; double d; int t; };
int mixlayout(struct Mix *m) {
    if (sizeof(struct Mix) != 16) return -1;
    if (m->c != 'x' || m->d != 2.5 || m->t != 77) return -2;
    m->t = 78;
    return 0;
}
/* s12cc caller -> LLVM-built byval callee: the callee mutates its
 * copy; our local must come back untouched (proves the caller-side
 * copy). */
extern int cmut(struct Pt p);
int drive_cmut(void) {
    struct Pt p; int r;
    p.x = 5; p.y = 6;
    r = cmut(p);
    if (p.x != 5 || p.y != 6) return -1;
    return r;
}
/* Issue #6: implicit int -> long long argument promotion.  A plain
 * int argument bound to a 64-bit parameter must widen to a full pair
 * (sign- or zero-extended); before the fix only the low word was
 * marshalled and the high word was whatever the register held. */
extern long long negate64(long long v);
int drive_promote(void) {
    int k; int m;
    k = 7; m = -5;
    if (mul64(k, 7) != 49ULL) return 1;
    if (negate64(m) != 5LL) return 2;
    if (negate64(2) != -2LL) return 3;
    return 0;
}
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
extern struct Pt mkpt(int x, int y);
struct Big { int a[6]; };
extern int bigmix(int,int,int,int,int,int,int,int, struct Big, int, int);
struct Mix { char c; double d; int t; };
extern int mixlayout(struct Mix *m);
extern int drive_cmut(void);
extern int drive_promote(void);
long long negate64(long long v) { return -v; }
int cmut(struct Pt p) { p.x = p.x + 100; return p.x + p.y; }
int cb(int x) { return x * 10; }
int main(void) {
    int fail = 0;
    if (add3(1,2,3) != 6) { fail = 1; printf("GATE add3 FAIL\n"); }
    if (sum9(1,2,3,4,5,6,7,8,9) != 45) { fail = 1; printf("GATE sum9 FAIL\n"); }
    if (mul64(0x100000001ULL, 7u) != 0x700000007ULL) { fail = 1; printf("GATE mul64 FAIL\n"); }
    if (call_back_twice(4) != 90) { fail = 1; printf("GATE callback FAIL\n"); }
    if (vsum(3, 10, 20, 30) != 60) { fail = 1; printf("GATE varargs FAIL\n"); }
    if (dmul(2.5, 4.0) != 10.0) { fail = 1; printf("GATE double FAIL\n"); }
    struct Pt p; p.x = 30; p.y = 12;
    if (ptsum(p) != 42) { fail = 1; printf("GATE struct-arg FAIL\n"); }
    struct Pt q = mkpt(7, 8);
    if (q.x != 7 || q.y != 8) { fail = 1; printf("GATE struct-ret FAIL\n"); }
    struct Big bg = {{21, 0, 0, 0, 0, 22}};
    if (bigmix(1,2,3,4,5,6,7,8, bg, 10, 11) != 73) { fail = 1; printf("GATE struct-stack-mix FAIL\n"); }
    if (drive_cmut() != 111) { fail = 1; printf("GATE struct-byval-copy FAIL\n"); }
    if (drive_promote() != 0) { fail = 1; printf("GATE arg-promote FAIL (%d)\n", drive_promote()); }
    struct Mix mx; mx.c = 'x'; mx.d = 2.5; mx.t = 77;
    if (sizeof(struct Mix) != 16 || mixlayout(&mx) != 0 || mx.t != 78) {
        fail = 1; printf("GATE struct-layout FAIL\n");
    }
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
