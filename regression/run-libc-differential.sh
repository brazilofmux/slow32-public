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
# A test either side cannot build is a failure, not a skip: three stdio
# tests sat here "skipped" on the self-hosted side for want of an include
# path, and the summary said 2 agree, 0 differ.
#
# A third leg, for the tests that ask for it: a test with HOSTLEG in its
# first lines is also built with the host's C compiler against the
# host's libc, and both libraries must print what that prints.  Two
# libraries written here can be wrong the same way -- and where one
# source is compiled into both (printf, strtod, the transcendentals)
# agreement says nothing about the library at all.  The host's is the
# independent one.  A HOSTLEG test prints only what the standard fixes:
# nothing that depends on the width of long, on a locale, on the text of
# an error message.  All three run in one time zone (TZ below), one with
# daylight time, so local-time conversions are compared too.
#
# And where the host cannot be asked -- the width of long, the words of
# an error message, a struct tm built by hand -- a test may have a
# .expect beside it: what it must print, written down.  Without one,
# code that both libraries build from one source (strtoul, strerror,
# strftime) is compared with itself.
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

# the self-hosted libc as the kit ships it: every object but crt0, archived
AR="$ROOT/tools/utilities/s32-ar"
SELF_LIBC="$W/libc.s32a"
SELF_MEMBERS=""
for o in "$L"/*.s32o; do
    [ "$(basename "$o")" = crt0.s32o ] || SELF_MEMBERS="$SELF_MEMBERS $o"
done
"$AR" rc "$SELF_LIBC" $SELF_MEMBERS >/dev/null 2>&1 || { echo "cannot archive $L" >&2; exit 1; }
HOSTCC="${HOSTCC:-cc}"
export TZ="${LIBC_TEST_TZ:-America/Chicago}"
TLIMIT="${LIBC_TEST_TIMEOUT:-120}"

pass=0; fail=0
for src in "$HERE"/libc-tests/*.c; do
    [ -f "$src" ] || continue
    tag="$(basename "$src" .c)"
    # arguments name the tests to run; none means all
    if [ $# -gt 0 ]; then
        want=0; for a in "$@"; do [ "$a" = "$tag" ] && want=1; done
        [ $want -eq 1 ] || continue
    fi

    # clang side
    if ! "$CLANG" -target slow32-unknown-none -S -emit-llvm -O2 \
            -I"$ROOT/runtime/include" "$src" -o "$W/c.ll" 2>/dev/null ||
       ! "$LLC" -mtriple=slow32-unknown-none -O2 "$W/c.ll" -o "$W/c.s" 2>/dev/null ||
       ! "$AS" "$W/c.s" "$W/c.s32o" >/dev/null 2>&1 ||
       ! "$LD" -o "$W/c.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/c.s32o" \
            "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1; then
        printf "  %-16s FAIL (clang side did not build)\n" "$tag"; fail=$((fail+1)); continue
    fi
    # self-hosted side
    if ! timeout 600 "$EMU" "$CC" "-I$ROOT/selfhost/stage08/include" "$src" "$W/s.s" >/dev/null 2>&1 ||
       ! "$AS" "$W/s.s" "$W/s.s32o" >/dev/null 2>&1 ||
       ! "$LD" -o "$W/s.s32x" --mmio 64K "$L/crt0.s32o" "$W/s.s32o" "$SELF_LIBC" \
            >/dev/null 2>&1; then
        printf "  %-16s FAIL (self-hosted side did not build)\n" "$tag"; fail=$((fail+1)); continue
    fi

    # in the scratch directory: a test that writes files leaves them there
    # ... and one with a .in beside it reads that as its standard input
    in=/dev/null; [ -f "${src%.c}.in" ] && in="${src%.c}.in"
    # ... and under a time limit: a library fault that makes a test loop
    # (getline returning 0 at end of file, say) is a failure, not a gate
    # that never ends
    (cd "$W" && timeout "$TLIMIT" "$RUN" -q "$W/c.s32x" < "$in" 2>&1 | grep -av "^HALT" > "$W/c.out"; [ "${PIPESTATUS[0]}" = 124 ] && echo "TIMED OUT after ${TLIMIT}s" >> "$W/c.out")
    (cd "$W" && timeout "$TLIMIT" "$RUN" -q "$W/s.s32x" < "$in" 2>&1 | grep -av "^HALT" > "$W/s.out"; [ "${PIPESTATUS[0]}" = 124 ] && echo "TIMED OUT after ${TLIMIT}s" >> "$W/s.out")
    legs=""
    if head -5 "$src" | grep -q HOSTLEG; then
        rm -f "$W/h" "$W/h.out"
        if ! "$HOSTCC" -std=gnu99 -D_GNU_SOURCE -w -o "$W/h" "$src" -lm >/dev/null 2>&1; then
            printf "  %-16s FAIL (host side did not build)\n" "$tag"; fail=$((fail+1)); continue
        fi
        # (the braces keep the shell's own "Abort trap" for a test that
        # ends in abort() off the terminal)
        { (cd "$W" && timeout "$TLIMIT" ./h < "$in" > "$W/h.out" 2>&1); } 2>/dev/null
        legs=", host too"
        if ! diff -q "$W/h.out" "$W/c.out" >/dev/null; then
            printf "  %-16s DIFFER (host libc / clang runtime)\n" "$tag"; fail=$((fail+1))
            diff "$W/h.out" "$W/c.out" | head -10 | sed 's/^/      /'
            diff -q "$W/h.out" "$W/s.out" >/dev/null ||
                { echo "      and host libc / self-hosted:"; diff "$W/h.out" "$W/s.out" | head -10 | sed 's/^/      /'; }
            continue
        fi
    fi
    if [ -f "${src%.c}.expect" ]; then
        legs="$legs, as expected"
        if ! diff -q "${src%.c}.expect" "$W/c.out" >/dev/null; then
            printf "  %-16s DIFFER (expected / clang runtime)\n" "$tag"; fail=$((fail+1))
            diff "${src%.c}.expect" "$W/c.out" | head -10 | sed 's/^/      /'
            continue
        fi
    fi
    if diff -q "$W/c.out" "$W/s.out" >/dev/null; then
        printf "  %-16s AGREE (%s lines%s)\n" "$tag" "$(wc -l < "$W/c.out" | tr -d ' ')" "$legs"
        pass=$((pass+1))
    else
        printf "  %-16s DIFFER\n" "$tag"; fail=$((fail+1))
        diff "$W/c.out" "$W/s.out" | head -10 | sed 's/^/      /'
    fi
done

echo ""
echo "libc differential: $pass agree, $fail differ"
[ "$fail" -eq 0 ]
