#!/bin/zsh
# build.sh [out]: the Fortran runtime and intrinsic modules for SLOW-32, from
# ~/llvm-project's flang-rt, with the flang in build-flang (docs/plans/flang.md).
set -e
export LC_ALL=C   # the archive's order is the glob's: the same on every host
HERE=${0:A:h}; ROOT=${HERE:h}; L=${LLVM_PROJECT:-$HOME/llvm-project}; O=${1:-$HERE/out}
# FLANG_BIN, S32_CXX, LIBCXX_INCLUDE, S32_AS, S32_AR, S32_RT_INCLUDE: where a piece is elsewhere (Dockerfile.toolchain).
B=${FLANG_BIN:-$L/build-flang/bin}; CXX=${S32_CXX:-$L/build/bin/clang++}; AS=${S32_AS:-$ROOT/tools/assembler/slow32asm}
LCX=${LIBCXX_INCLUDE:-$L/build-slow32-libcxx/install/include/c++/v1}; RT=$L/flang-rt/lib/runtime; RI=${S32_RT_INCLUDE:-$ROOT/runtime/include}
mkdir -p $O/rt $O/mod
FL=(-I$HERE/shim -I$L/flang-rt/include -I$L/flang/include -I$L/flang-rt/lib)
for f in $RT/*.cpp $L/flang/lib/Decimal/*.cpp; do n=${f:t:r}
  case $n in trampoline|io-stmt-minimal|io-api-gpu|io-api-server) continue;; esac   # W^X; GPU only
  $CXX -target slow32-unknown-none -std=c++17 -O2 -fno-exceptions -fno-rtti -nostdinc++ -isystem $LCX -isystem $RI $FL -DFLANG_RT_BUILD -DRT_NO_RPC_SERVER -DRT_USE_PSEUDO_LOCK=1 -DFLANG_LITTLE_ENDIAN=1 -ffile-prefix-map=$L/=llvm-project/ -S $f -o $O/rt/$n.s
  $AS $O/rt/$n.s $O/rt/$n.s32o >/dev/null
done
rm -f $O/libflangrt.s32a; ${S32_AR:-$ROOT/tools/utilities/s32-ar} rc $O/libflangrt.s32a $O/rt/*.s32o
todo=(__fortran_builtins __fortran_ieee_exceptions __fortran_type_info ieee_arithmetic ieee_exceptions ieee_features iso_c_binding iso_fortran_env_impl iso_fortran_env)
for pass in 1 2 3; do left=(); for m in $todo; do
  $B/flang -fc1 -triple slow32-unknown-none -cpp -fsyntax-only -module-dir $O/mod -fintrinsic-modules-path $O/mod -fno-reformat $RT/$m.f90 >/dev/null 2>&1 && [ -f $O/mod/$m.mod ] || left+=$m
done; todo=($left); [ ${#todo} -eq 0 ] && break; done
[ ${#todo} -eq 0 ] || { print -u2 "modules not built: $todo"; exit 1; }
print "runtime: $(ls $O/rt/*.s32o | wc -l | tr -d ' ') objects; modules: $(ls $O/mod/*.mod | wc -l | tr -d ' ')"
