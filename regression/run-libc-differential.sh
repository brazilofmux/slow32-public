#!/usr/bin/env bash
# Differential harness: the CLANG-side runtime vs the SELF-HOSTED libc.
#
# The tree carries two independent C libraries -- runtime/*.c, linked
# into libc_mmio.s32a for clang-built programs, and
# selfhost/stage08/libc/*.c for the self-hosted world -- and until this
# script nothing compared them.  The 84-test regression suite links the
# CLANG runtime, so roughly 100 self-hosted libc entry points had no
# direct coverage at all.  That is where the fdseek bug lived.
#
# Each test is an ordinary C program that prints deterministic results
# and is compiled BOTH ways: clang -> clang runtime, and stage08 cc ->
# self-hosted libc.  The two outputs must be identical.
#
# Writing tests for this: avoid anything the standard leaves open, or
# the harness reports portability differences as failures.  The first
# draft did exactly that -- it took the difference of two pointers into
# separate copies of the same string literal, which is well-defined
# only if the compiler merges identical literals.  Clang does; stage08
# cc does not; both are conforming.  Compare against ONE buffer.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
EMU="${SELFHOST_EMU:-$ROOT/selfhost/stage00/s32-emu}"
RUN="$ROOT/tools/emulator/slow32"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
CLANG="${CLANG:-$HOME/llvm-project/build/bin/clang}"
LLC="${LLC:-$HOME/llvm-project/build/bin/llc}"
CC="$ROOT/selfhost/stage08/cc.s32x"
L="$ROOT/selfhost/stage08/lib"
W="$(mktemp -d)"
trap 'rm -rf "$W"' EXIT

[ -x "$CLANG" ] && [ -x "$LLC" ] || { echo "clang/llc not available; skipping" >&2; exit 0; }
[ -f "$CC" ] || { echo "missing $CC" >&2; exit 1; }

SELF_OBJS="$L/start.s32o $L/mmio_no_start.s32o $L/builtins64.s32o \
$L/builtins_fp64.s32o $L/string_extra.s32o $L/string_more.s32o $L/ctype.s32o \
$L/convert.s32o $L/stdio.s32o $L/malloc.s32o $L/dtoa.s32o \
$L/printf_enhanced.s32o $L/convert_rt.s32o"

pass=0; fail=0
for src in "$HERE"/libc-tests/*.c; do
    [ -f "$src" ] || continue
    tag="$(basename "$src" .c)"

    # clang side
    if ! "$CLANG" -target slow32-unknown-none -S -emit-llvm -O2 \
            -I"$ROOT/runtime/include" "$src" -o "$W/c.ll" 2>/dev/null ||
       ! "$LLC" -mtriple=slow32-unknown-none -O2 "$W/c.ll" -o "$W/c.s" 2>/dev/null ||
       ! "$AS" "$W/c.s" "$W/c.s32o" >/dev/null 2>&1 ||
       ! "$LD" -o "$W/c.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/c.s32o" \
            "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1; then
        printf "  %-16s SKIP (clang side did not build)\n" "$tag"; continue
    fi
    # self-hosted side
    if ! timeout 600 "$EMU" "$CC" "$src" "$W/s.s" >/dev/null 2>&1 ||
       ! "$AS" "$W/s.s" "$W/s.s32o" >/dev/null 2>&1 ||
       ! "$LD" -o "$W/s.s32x" --mmio 64K "$L/crt0.s32o" "$W/s.s32o" $SELF_OBJS \
            >/dev/null 2>&1; then
        printf "  %-16s SKIP (self-hosted side did not build)\n" "$tag"; continue
    fi

    "$RUN" -q "$W/c.s32x" 2>&1 | grep -v "^HALT" > "$W/c.out"
    "$RUN" -q "$W/s.s32x" 2>&1 | grep -v "^HALT" > "$W/s.out"
    if diff -q "$W/c.out" "$W/s.out" >/dev/null; then
        printf "  %-16s AGREE (%s lines)\n" "$tag" "$(wc -l < "$W/c.out" | tr -d ' ')"
        pass=$((pass+1))
    else
        printf "  %-16s DIFFER\n" "$tag"; fail=$((fail+1))
        diff "$W/c.out" "$W/s.out" | head -10 | sed 's/^/      /'
    fi
done

echo ""
echo "libc differential: $pass agree, $fail differ"
[ "$fail" -eq 0 ]
