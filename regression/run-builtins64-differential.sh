#!/bin/bash

# selfhost/stage08/builtins64.s against runtime/builtins.c, one program.
#
# WHY THIS EXISTS.  builtins64.s and runtime/builtins.c define the same four
# 64-bit divide routines, and which one a program gets is decided by LINK
# ORDER, not by anything visible in the source: builtins64.s32o goes in ahead
# of libs32.s32a and the linker takes the first definition, silently.  So the
# assembly copy is what actually runs on every host without LLVM -- and it is
# the copy nothing tested.  GitHub #30: it had never been given the 32-bit-
# divisor fast path the C has had all along, so every 64-bit divide there ran
# 64 shift-subtract rounds.  Four years of green suites never said a word,
# because the suites are built by clang, which resolves the C copy.
#
# The check exploits exactly that: build ONE clang-compiled program that calls
# __udivdi3 / __umoddi3 / __divdi3 / __moddi3 directly, link it twice -- once
# with builtins64.s32o in front, once without -- and compare a checksum over
# ~85000 vectors.  The two link lines differ in nothing else, so a mismatch is
# a real disagreement between the two implementations of the same routine.
#
# The vectors cover every class the dispatch splits on, since a fast path is
# exactly where a divide routine goes wrong: 26x26 edge pairs (0, 1, 2^31,
# 2^32, 2^63, ~0, and neighbours), 40000 PRNG pairs with both operands
# randomly narrowed to any width from 1 to 64 bits, then every power of ten to
# 10^18 and every power of two to 2^63 as divisors -- what COBOL decimal
# arithmetic actually divides by.
#
# SELF-VALIDATING.  The first argument points at any builtins64.s, so the
# check can be proved able to fail rather than assumed to:
#
#   sed '/jal  r31, __udivmoddi3/a\    addi r1, r1, 1' \
#       selfhost/stage08/builtins64.s > /tmp/faulted.s
#   ./run-builtins64-differential.sh /tmp/faulted.s    # must FAIL
#   ./run-builtins64-differential.sh                   # must PASS
#
# Measured on kagura: clean 18191a74 both sides; that fault gives 129113dd.
#
# EXIT CODES, following run-differential.sh: 0 the check ran and the two
# agree, 1 they disagree, 2 THE CHECK COULD NOT RUN -- no clang for the
# slow32 target, so nothing was compared.  2 rather than 0 because a host
# without LLVM is precisely the host this bug lives on, and it must not
# inherit a pass it did not earn.
#
# Usage: ./run-builtins64-differential.sh [builtins64.s]
# Env: CLANG, LLC (default ~/llvm-project/build/bin, then PATH, then the
#      slow32:toolchain container), EMU, KEEP=1.

set -eu

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
SRC="${1:-$ROOT/selfhost/stage08/builtins64.s}"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
WORK="$(mktemp -d)"
trap '[ -n "${KEEP:-}" ] || rm -rf "$WORK"' EXIT

[ -f "$SRC" ] || { echo "no such builtins source: $SRC" >&2; exit 2; }

cat > "$WORK/b64diff.c" <<'EOF'
/* One program, two link lines.  Calls the builtins by name so no amount of
 * constant folding can turn a divide into something else. */
#include <stdio.h>

typedef unsigned long long u64;
typedef long long i64;
typedef unsigned int u32;

extern u64 __udivdi3(u64, u64);
extern u64 __umoddi3(u64, u64);
extern i64 __divdi3(i64, i64);
extern i64 __moddi3(i64, i64);

static u64 st = 0x243F6A8885A308D3ULL;
static u64 rnd(void) { u64 x = st; x ^= x << 13; x ^= x >> 7; x ^= x << 17; st = x; return x; }

static u32 ck = 2166136261u;
static void mix32(u32 v) { ck = (ck ^ v) * 16777619u; }
static void mix64(u64 v) { mix32((u32)v); mix32((u32)(v >> 32)); }

static void probe(u64 n, u64 d) {
    mix64(__udivdi3(n, d));
    mix64(__umoddi3(n, d));
    mix64((u64)__divdi3((i64)n, (i64)d));
    mix64((u64)__moddi3((i64)n, (i64)d));
}

static const u64 edge[] = {
    0ULL, 1ULL, 2ULL, 3ULL, 7ULL, 10ULL, 100ULL, 1000000000ULL,
    0x7FFFFFFFULL, 0x80000000ULL, 0xFFFFFFFEULL, 0xFFFFFFFFULL,
    0x100000000ULL, 0x100000001ULL, 0x123456789ULL, 0xFFFFFFFF00000000ULL,
    0x7FFFFFFFFFFFFFFFULL, 0x8000000000000000ULL, 0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFE00000001ULL, 0x0000000100000000ULL, 0x00000000FFFFFFFFULL,
    0x5555555555555555ULL, 0xAAAAAAAAAAAAAAAAULL, 0x000000007FFFFFFFULL,
    0x8000000000000001ULL,
};
#define NEDGE ((int)(sizeof edge / sizeof edge[0]))

int main(void) {
    int i, j, k;
    for (i = 0; i < NEDGE; i++)
        for (j = 0; j < NEDGE; j++)
            probe(edge[i], edge[j]);

    /* Widths varied so every divisor class is hit: den_hi == 0 with den_lo
     * below and above 2^31, den_hi != 0, and den > num. */
    for (k = 0; k < 40000; k++) {
        u64 n = rnd() >> (unsigned)(rnd() & 63);
        u64 d = rnd() >> (unsigned)(rnd() & 63);
        probe(n, d);
        probe(d, n);
    }

    /* The divisors decimal arithmetic actually uses. */
    {
        u64 p10 = 1;
        for (i = 0; i < 19; i++) {
            for (k = 0; k < 200; k++) probe(rnd(), p10);
            p10 = p10 * 10ULL;
        }
        for (i = 0; i < 64; i++) {
            u64 p2 = 1ULL << i;
            for (k = 0; k < 100; k++) probe(rnd(), p2);
        }
    }

    printf("b64 checksum %08x\n", ck);
    return 0;
}
EOF

# --- find a clang that targets slow32: local build, PATH, then container ---
CLANG="${CLANG:-$HOME/llvm-project/build/bin/clang}"
LLC="${LLC:-$HOME/llvm-project/build/bin/llc}"
compile_local() {
    "$CLANG" -target slow32-unknown-none -S -emit-llvm -O2 -I"$ROOT/runtime/include" \
        "$WORK/b64diff.c" -o "$WORK/b64diff.ll" &&
    "$LLC" -mtriple=slow32-unknown-none "$WORK/b64diff.ll" -o "$WORK/b64diff.s"
}
compile_container() {
    local eng
    eng="$(command -v podman || command -v docker)" || return 1
    "$eng" image inspect slow32:toolchain >/dev/null 2>&1 || return 1
    "$eng" run --rm -v "$WORK":/w -v "$ROOT/runtime/include":/inc:ro slow32:toolchain \
        bash -c 'clang -target slow32-unknown-none -S -emit-llvm -O2 -I/inc \
                     /w/b64diff.c -o /w/b64diff.ll &&
                 llc -mtriple=slow32-unknown-none /w/b64diff.ll -o /w/b64diff.s'
}
if [ -x "$CLANG" ] && [ -x "$LLC" ]; then
    compile_local >/dev/null 2>&1 || { echo "clang failed" >&2; exit 2; }
elif compile_container >/dev/null 2>&1; then
    :
else
    echo "no clang for slow32 (build one, or build the slow32:toolchain image)" >&2
    echo "SKIPPED - the comparison never ran" >&2
    exit 2
fi

AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
"$AS" "$WORK/b64diff.s" "$WORK/b64diff.s32o" >/dev/null
"$AS" "$SRC" "$WORK/builtins64.s32o" >/dev/null

# The only difference between the two link lines is builtins64.s32o, which
# goes in front of the archive and therefore wins every symbol it defines.
"$LD" -o "$WORK/asm.s32x" "$ROOT/runtime/crt0.s32o" "$WORK/b64diff.s32o" \
    "$WORK/builtins64.s32o" "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a"
"$LD" -o "$WORK/c.s32x" "$ROOT/runtime/crt0.s32o" "$WORK/b64diff.s32o" \
    "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a"

run() { "$EMU" "$1" | awk '/b64 checksum/{s=$3} /Instructions executed/{n=$3} END{print s, n}'; }
read -r asum ains <<EOF
$(run "$WORK/asm.s32x")
EOF
read -r csum cins <<EOF
$(run "$WORK/c.s32x")
EOF

printf '  %-28s %-10s %14s\n' source checksum instructions
printf '  %-28s %-10s %14s\n' 'runtime/builtins.c (libs32)' "$csum" "$cins"
printf '  %-28s %-10s %14s\n' "$(basename "$SRC") (assembly)" "$asum" "$ains"

if [ -z "$asum" ] || [ -z "$csum" ]; then
    echo "FAIL: a run produced no checksum" >&2
    exit 1
fi
if [ "$asum" != "$csum" ]; then
    echo "FAIL: the two implementations disagree" >&2
    exit 1
fi
echo "PASS: identical over the whole corpus"
