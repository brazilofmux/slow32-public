#!/bin/zsh
# run-tests.sh [mod-dir]: flang's own tests, each self-checking, on every engine
# that can run them: abi (Fortran's layouts against clang's, with C on the other
# side), wide (what is wider than index), oor (OUT_OF_RANGE with no wider real) and
# files (common blocks across files).
# Needs flang/build.sh first.
# Exit 0 only if each prints its success line everywhere.  ~10 s.
HERE=${0:A:h}; FL=${HERE:h}; ROOT=${FL:h}; L=${LLVM_PROJECT:-$HOME/llvm-project}
B=$L/build-flang/bin; R=$ROOT/runtime; LX=$L/build-slow32-libcxx/install/lib
AS=$ROOT/tools/assembler/slow32asm; M=${1:-$FL/out/mod}
ENGINES=($ROOT/tools/dbt/slow32-dbt $ROOT/tools/emulator/slow32-fast $ROOT/tools/emulator/slow32)
T=$(mktemp -d "${TMPDIR:-/tmp}/flang-tests.XXXXXX"); cd $T || exit 2
bad=0
one() { # name extra-objects...
  local n=$1 e out; shift
  if ! $B/flang -fc1 -triple slow32-unknown-none -O2 -fintrinsic-modules-path $M -module-dir . -emit-llvm $HERE/$n.f90 -o $n.ll \
    || ! $B/llc -mtriple=slow32-unknown-none -O2 $n.ll -o $n.s \
    || ! $AS $n.s $n.s32o >/dev/null \
    || ! $ROOT/tools/linker/s32-ld -o $n.s32x --mmio 64K --heap-size 16M $R/crt0.s32o $n.s32o "$@" $FL/out/libflangrt.s32a $LX/libc++.a $LX/libc++abi.a $R/libc_mmio.s32a $R/libs32.s32a $R/libs32cxx.s32a
  then print "FAIL $n: did not build"; bad=$((bad+1)); return; fi
  for e in $ENGINES; do
    out=$(timeout 60 $e $n.s32x 2>&1)   # the slower engines report statistics after it
    if print -r -- "$out" | grep -qx "$n-ok"; then print "ok   $n ${e:t}"; else print "FAIL $n ${e:t}: ${out[1,100]}"; bad=$((bad+1)); fi
  done
}
$L/build/bin/clang -target slow32-unknown-none -O2 -S -emit-llvm -I$R/include $HERE/abi-driver.c -o driver.ll \
  && $L/build/bin/llc -mtriple=slow32-unknown-none -O2 driver.ll -o driver.s && $AS driver.s driver.s32o >/dev/null || { print "FAIL abi-driver"; exit 1; }
one abi driver.s32o
one wide
one oor
$B/flang -fc1 -triple slow32-unknown-none -O2 -fintrinsic-modules-path $M -module-dir . -emit-llvm $HERE/files2.f90 -o files2.ll \
  && $B/llc -mtriple=slow32-unknown-none -O2 files2.ll -o files2.s && $AS files2.s files2.s32o >/dev/null || { print "FAIL files2: did not build"; bad=$((bad+1)); }
one files files2.s32o
print "flang tests: $bad failed"
[ $bad -eq 0 ]
