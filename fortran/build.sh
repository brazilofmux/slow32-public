#!/bin/bash
# Build f77 (host) and libf77 (guest).
#
# fortran/ is in the tree's ordinary universe: the host C compiler builds
# the compiler, and the host SLOW-32 toolchain (LLVM) builds the runtime
# -- runtime/build.sh, which honours LLVM_BIN, S32_AS and S32_RT_INCLUDE.
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$HERE/out"
CC="${CC:-cc}"
$CC -O1 -w -o "$HERE/out/f77" "$HERE/src/f77.c"
echo "built: $HERE/out/f77"
"$HERE/runtime/build.sh"
