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
trap 'rm -rf "$W"' EXIT

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
        "$HERE/oracle.sh" "$f" > "$W/$b.want" 2>/dev/null; wrc=$?
        # our compiler -> .s -> .s32o -> .s32x -> emulator
        if ! "$F77" "$f" "$W/$b.s" >"$W/$b.cc.log" 2>&1; then
            report "diff:$b" 1 "f77 compile"; continue
        fi
        "$AS" "$W/$b.s" "$W/$b.s32o" >/dev/null 2>&1 || { report "diff:$b" 1 "assemble"; continue; }
        # --mmio + libc_mmio is what propagates the guest exit status out
        # of the emulator, which is how STOP n is checked.
        "$LD" -o "$W/$b.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/$b.s32o" \
              "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" \
              >/dev/null 2>&1 || { report "diff:$b" 1 "link"; continue; }
        "$EMU" "$W/$b.s32x" 2>/dev/null \
            | grep -vE "^Starting execution|^HALT at|^$|^Program halted|^Instructions|^Cycles|^Wall|^Performance|^MMIO|^Exit code" \
            > "$W/$b.got"
        "$EMU" "$W/$b.s32x" >/dev/null 2>&1; grc=$?
        if diff -q "$W/$b.want" "$W/$b.got" >/dev/null 2>&1 && [ "$wrc" = "$grc" ]; then
            report "diff:$b" 0
        else
            report "diff:$b" 1 "output/exit differs"
            diff "$W/$b.want" "$W/$b.got" | head -8
        fi
    done
fi

# --- Gate 4: no libm side door --------------------------------------
# slow32-dbt links host libm (LDFLAGS = -lm) and installs an intercept
# for any of ~37 math symbols it finds in the GUEST's symbol table,
# replacing guest execution with a native host call.  That is a real
# trapdoor: sbasic.s32x, for instance, carries sqrt/atan2/floor and so
# runs them on the host under the DBT.
#
# Fortran must not reach through it.  Every FP operation we emit is a
# SLOW-32 hardware instruction, so a Fortran binary should contain NO
# interceptable math symbol at all.  This gate asserts that, because it
# is the kind of property that regresses silently the first time
# someone lowers an intrinsic to a libcall.
INTERCEPTABLE="sqrt sqrtf sin cos tan asin acos atan sinh cosh tanh exp log log10 ceil floor round trunc fabs fmod fmodf sinf cosf tanf asinf acosf atanf sinhf coshf tanhf expf logf log10f ceilf floorf roundf truncf fabsf"
leaked=""
for f in "$HERE"/f77/*.f; do
    [ -e "$f" ] || continue
    b="$(basename "$f" .f)"
    "$F77" "$f" "$W/$b.lm.s" >/dev/null 2>&1 || continue
    "$AS" "$W/$b.lm.s" "$W/$b.lm.s32o" >/dev/null 2>&1 || continue
    "$LD" -o "$W/$b.lm.s32x" --mmio 64K "$ROOT/runtime/crt0.s32o" "$W/$b.lm.s32o" \
          "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a" >/dev/null 2>&1 || continue
    for n in $INTERCEPTABLE; do
        if strings -a "$W/$b.lm.s32x" 2>/dev/null | grep -qx "$n"; then
            leaked="$leaked $b:$n"
        fi
    done
done
if [ -z "$leaked" ]; then
    report "no-libm-sidedoor" 0
else
    report "no-libm-sidedoor" 1 "interceptable symbols linked in"
    echo "     leaked:$leaked"
fi

echo
echo "$PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
