#!/bin/bash
# fortran/ test harness.
#
# Gate 1 (backend-slice): drives the COPIED SLOW-32 backend with
# hand-built HIR and no Fortran frontend, then runs the result on the
# emulator.  This is the load-bearing test for the copy: if a re-sync
# from selfhost/ breaks the frontend contract in f77_contract.h, this
# fails immediately and specifically, rather than surfacing later as a
# mysterious miscompile in Fortran code.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
FDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$FDIR/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"

W="$(mktemp -d /tmp/f77-tests.XXXXXX)"
# The oracle runs on a COPY in a $HOME-based scratch dir: podman's
# macOS VM does not share /tmp, and a test that OPENs files must never
# scribble them into tests/f77/ (the mounted directory).
OW="$(mktemp -d "$HOME/.f77-oracle.XXXXXX")"
trap 'rm -rf "$W" "$OW"' EXIT

PASS=0; FAIL=0

report() {
    if [ "$2" = "0" ]; then printf "  %-24s PASS\n" "$1:"; PASS=$((PASS+1))
    else printf "  %-24s FAIL%s\n" "$1:" "${3:+ ($3)}"; FAIL=$((FAIL+1)); fi
}

# --- Gate 1: backend slice -------------------------------------------
if ! gcc -I"$FDIR/src" -O1 -w -o "$W/slice" "$HERE/backend_slice.c" 2>"$W/cc.log"; then
    report "backend-slice" 1 "host build"
else
    "$W/slice" "$W/slice.s" 2>/dev/null
    "$AS" "$W/slice.s" "$W/slice.s32o" >/dev/null 2>&1
    "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -O1 \
        -I"$ROOT/runtime/include" "$HERE/backend_slice_drv.c" -o "$W/drv.ll" 2>/dev/null
    "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/drv.ll" -o "$W/drv.s" 2>/dev/null
    "$AS" "$W/drv.s" "$W/drv.s32o" >/dev/null 2>&1
    "$LD" -o "$W/slice.s32x" "$ROOT/runtime/crt0.s32o" "$W/drv.s32o" "$W/slice.s32o" \
        "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1
    "$EMU" "$W/slice.s32x" 2>/dev/null \
        | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO" \
        > "$W/slice.out"
    if diff -q "$W/slice.out" "$HERE/backend_slice.expected" >/dev/null 2>&1; then
        report "backend-slice" 0
    else
        report "backend-slice" 1 "output mismatch"
        diff "$HERE/backend_slice.expected" "$W/slice.out" | head -10
    fi
fi

# --- Gate 2: card image + tokenizer ----------------------------------
if ! gcc -I"$FDIR/src" -O1 -w -o "$W/lexdump" "$HERE/lexdump.c" 2>"$W/lex.log"; then
    report "lex-torture" 1 "host build"
else
    "$W/lexdump" "$HERE/torture.f" > "$W/torture.out" 2>&1
    if diff -q "$W/torture.out" "$HERE/torture.expected" >/dev/null 2>&1; then
        report "lex-torture" 0
    else
        report "lex-torture" 1 "token stream mismatch"
        diff "$HERE/torture.expected" "$W/torture.out" | head -12
    fi
fi

# --- Gate 3: differential vs the gfortran oracle ----------------------
# Each tests/f77/*.f is run under gfortran-in-a-container (expected) and,
# once fortran/out/f77 exists, under our compiler on the emulator
# (actual).  stdout and exit status must both match.  gfortran sends the
# `STOP n` message to stderr and the code to the exit status, so stdout
# stays clean for diffing.
F77="$FDIR/out/f77"
if [ ! -x "$F77" ]; then
    printf "  %-24s SKIP (no compiler yet -- milestone 3)\n" "differential:"
elif ! "$HERE/oracle.sh" --check >/dev/null 2>&1 && \
     ! podman image exists slow32:fortran-oracle 2>/dev/null; then
    printf "  %-24s SKIP (oracle image absent)\n" "differential:"
else
    for f in "$HERE"/f77/*.f; do
        [ -e "$f" ] || continue
        b="$(basename "$f" .f)"
        # A sibling <name>.in is the program's stdin (for READ tests),
        # fed identically to the oracle and to the emulator.  /dev/null
        # otherwise, so a runaway READ cannot hang on the terminal.
        IN="/dev/null"
        [ -f "$HERE/f77/$b.in" ] && IN="$HERE/f77/$b.in"
        cp "$f" "$OW/$b.f"
        "$HERE/oracle.sh" "$OW/$b.f" < "$IN" > "$W/$b.want" 2>/dev/null; wrc=$?
        # our compiler -> .s -> .s32o -> .s32x -> emulator
        if ! "$F77" "$f" "$W/$b.s" >"$W/$b.cc.log" 2>&1; then
            report "diff:$b" 1 "f77 compile"; continue
        fi
        "$AS" "$W/$b.s" "$W/$b.s32o" >/dev/null 2>&1 || { report "diff:$b" 1 "assemble"; continue; }
        # --mmio + libc_mmio is what propagates the guest exit status out
        # of the emulator, which is how STOP n is checked.
        "$LD" -o "$W/$b.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/$b.s32o" \
              "$FDIR/runtime/libf77.s32o" \
              "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" \
              >/dev/null 2>&1 || { report "diff:$b" 1 "link"; continue; }
        # cwd inside $W so files a test OPENs land in scratch space.
        (cd "$W" && "$EMU" "$W/$b.s32x" < "$IN" 2>/dev/null) \
            | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO|^Exit code" \
            > "$W/$b.got"
        (cd "$W" && "$EMU" "$W/$b.s32x" < "$IN" >/dev/null 2>&1); grc=$?
        # Agreement is necessary but NOT sufficient.  Every program in
        # f77/ is self-checking and ends in STOP 0, so a non-zero exit
        # means an assertion tripped -- and if the oracle trips the same
        # one, a plain diff calls that a PASS while the test has in fact
        # stopped early and never exercised what it was written for.
        # That happened: slice6 asserted FLAT(2)==21 against a fill of
        # I+10*J, stopped at its second check on BOTH compilers, and
        # reported PASS for several commits.  So require exit 0 too.
        if ! diff -q "$W/$b.want" "$W/$b.got" >/dev/null 2>&1 || [ "$wrc" != "$grc" ]; then
            report "diff:$b" 1 "output/exit differs (ours=$grc oracle=$wrc)"
            diff "$W/$b.want" "$W/$b.got" | head -8
        elif [ "$grc" != "0" ]; then
            report "diff:$b" 1 "both stopped early at STOP $grc -- test never completed"
        else
            report "diff:$b" 0
        fi
    done
fi

# --- Report: math libcalls emitted -----------------------------------
# INFORMATIONAL, not a failure.  fortran/ lives in the tree's ordinary
# universe, not selfhost's closed one: the compiler may use the host to
# target SLOW-32, and the emulators may use their environment to run
# SLOW-32 code.  slow32-dbt linking host libm and intercepting ~37 math
# symbols is therefore sanctioned, not a cheat -- sbasic.s32x carries
# sqrt/atan2/floor and runs them on the host under the DBT by design.
#
# What this reports is a code-quality fact worth watching: today f77
# needs NO math libcall, because every FP operation it emits is a
# SLOW-32 hardware instruction.  EXP, LOG, ATAN2, the trig functions
# and real-exponent ** have no instruction behind them, so when they
# land this list will legitimately grow.  It is printed rather than
# enforced so that adding them is not blocked by a rule that was never
# this directory's to begin with.
INTERCEPTABLE="sqrt sqrtf sin cos tan asin acos atan sinh cosh tanh exp log log10 ceil floor round trunc fabs fmod fmodf sinf cosf tanf asinf acosf atanf sinhf coshf tanhf expf logf log10f ceilf floorf roundf truncf fabsf"
leaked=""
for f in "$HERE"/f77/*.f; do
    [ -e "$f" ] || continue
    b="$(basename "$f" .f)"
    "$F77" "$f" "$W/$b.lm.s" >/dev/null 2>&1 || continue
    "$AS" "$W/$b.lm.s" "$W/$b.lm.s32o" >/dev/null 2>&1 || continue
    "$LD" -o "$W/$b.lm.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/$b.lm.s32o" \
          "$FDIR/runtime/libf77.s32o" \
          "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1 || continue
    for n in $INTERCEPTABLE; do
        if strings -a "$W/$b.lm.s32x" 2>/dev/null | grep -qx "$n"; then
            leaked="$leaked $b:$n"
        fi
    done
done
if [ -z "$leaked" ]; then
    printf "  %-24s none (all FP is hardware instructions)\n" "math-libcalls:"
else
    printf "  %-24s%s\n" "math-libcalls:" "$leaked"
fi

echo
echo "$PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
