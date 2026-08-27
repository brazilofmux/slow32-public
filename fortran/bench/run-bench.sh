#!/bin/bash
# Head-to-head: the same LINPACK kernel compiled by f77 and by clang,
# both targeting SLOW-32, so the comparison measures CODE GENERATION
# rather than ISA or algorithm.
#
# Instruction count is the primary metric: it is deterministic and free
# of host noise.  Wall time under slow32-dbt is reported alongside it.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
FDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$FDIR/.." && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
FAST="$ROOT/tools/emulator/slow32-fast"
DBT="$ROOT/tools/dbt/slow32-dbt"
OPT="${OPT:--O2}"
W="$(mktemp -d /tmp/f77bench.XXXXXX)"
trap 'rm -rf "$W"' EXIT

link() { "$LD" -o "$1" --mmio 64K "$ROOT/runtime/crt0.s32o" "$2" \
        "$FDIR/runtime/libf77.s32o" "$ROOT/runtime/libc_mmio.s32a" \
        "$ROOT/runtime/libs32.s32a" >/dev/null; }

# --- f77 ---
"$FDIR/out/f77" "$HERE/linpack.f" "$W/f.s" >/dev/null 2>&1 || { echo "f77 compile failed"; exit 1; }
"$AS" "$W/f.s" "$W/f.s32o" >/dev/null || exit 1
link "$W/f.s32x" "$W/f.s32o" || exit 1

# --- clang ---
"$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm $OPT \
    -nostdinc -fno-builtin -I"$ROOT/runtime/include" \
    "$HERE/linpack.c" -o "$W/c.ll" 2>/dev/null || { echo "clang failed"; exit 1; }
"$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/c.ll" -o "$W/c.s" || exit 1
"$AS" "$W/c.s" "$W/c.s32o" >/dev/null || exit 1
link "$W/c.s32x" "$W/c.s32o" || exit 1

count() { "$FAST" "$1" 2>/dev/null | grep -oE "Instructions executed: [0-9]+" | grep -oE "[0-9]+"; }
check()  { "$FAST" "$1" >/dev/null 2>&1; echo $?; }

fi_=$(count "$W/f.s32x"); fr=$(check "$W/f.s32x")
ci=$(count "$W/c.s32x"); cr=$(check "$W/c.s32x")

echo "correctness:  f77 exit=$fr   clang exit=$cr   (0 = residual check passed)"
[ "$fr" = "0" ] && [ "$cr" = "0" ] || { echo "ABORT: a build did not verify"; exit 1; }
echo
printf "instructions: f77   %12d\n" "$fi_"
printf "              clang %12d\n" "$ci"
python3 -c "print('              ratio  %.3fx  (f77 / clang)' % ($fi_/$ci))"
echo
echo "text size:    f77   $(python3 -c "
import struct,sys
d=open('$W/f.s32x','rb').read()
print(len(d))")  bytes (image)"
echo "              clang $(python3 -c "
d=open('$W/c.s32x','rb').read()
print(len(d))")  bytes (image)"
echo
echo "wall time under slow32-dbt (best of 5):"
for tag in f c; do
    best=""
    for i in 1 2 3 4 5; do
        s=$(python3 -c "import time;print(time.time())")
        "$DBT" "$W/$tag.s32x" >/dev/null 2>&1
        e=$(python3 -c "import time;print(time.time())")
        t=$(python3 -c "print($e-$s)")
        best=$(python3 -c "
b='$best'
print(min(float(b),$t) if b else $t)")
    done
    name=$([ $tag = f ] && echo "f77  " || echo "clang")
    printf "              %s %8.4f s\n" "$name" "$best"
done
