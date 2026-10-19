#!/bin/bash
# libc++ (LLVM's C++ standard library) for SLOW-32, from the llvm-project
# tree the backend lives in -- the same LLVM, so no second pin to track.
#
#   scripts/build-libcxx.sh            configure and generate the headers
#
# Output: $BUILD/include/c++/v1, the header tree C++ programs compile
# against, ahead of runtime/include:
#
#   clang++ -target slow32-unknown-none -nostdinc++ \
#       -isystem $BUILD/include/c++/v1 -isystem runtime/include \
#       -fexceptions -frtti ...
#
# Configured freestanding: no threads, monotonic clock, filesystem, random
# device, locales or wide characters; type_info compared by name, as
# runtime/cxa_typeinfo.c does; the C++ ABI (exceptions, RTTI, new/delete)
# is the runtime's own (LIBCXX_CXX_ABI=none).  The headers are not in git;
# this regenerates them.  The compiled half (libc++.a) is not built yet.
#
# Env: LLVM_PROJECT (default ~/llvm-project), LLVM_BIN (default
# $LLVM_PROJECT/build/bin), BUILD (default $LLVM_PROJECT/build-slow32-libcxx).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LLVM_PROJECT="${LLVM_PROJECT:-$HOME/llvm-project}"
LLVM_BIN="${LLVM_BIN:-$LLVM_PROJECT/build/bin}"
BUILD="${BUILD:-$LLVM_PROJECT/build-slow32-libcxx}"

# The clang driver assembles with slow32asm and links with s32-ld, by name.
export PATH="$ROOT/tools/assembler:$ROOT/tools/linker:$ROOT/tools/utilities:$PATH"

cmake -G Ninja -S "$LLVM_PROJECT/runtimes" -B "$BUILD" \
    -DLLVM_ENABLE_RUNTIMES=libcxx \
    -DCMAKE_C_COMPILER="$LLVM_BIN/clang" -DCMAKE_CXX_COMPILER="$LLVM_BIN/clang++" \
    -DCMAKE_C_COMPILER_TARGET=slow32-unknown-none \
    -DCMAKE_CXX_COMPILER_TARGET=slow32-unknown-none \
    -DCMAKE_SYSTEM_NAME=Generic -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
    -DCMAKE_C_FLAGS="-isystem $ROOT/runtime/include" \
    -DCMAKE_CXX_FLAGS="-isystem $ROOT/runtime/include" \
    -DCMAKE_INSTALL_PREFIX="$BUILD/install" \
    -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_STATIC=ON -DLIBCXX_CXX_ABI=none \
    -DLIBCXX_ENABLE_THREADS=OFF -DLIBCXX_ENABLE_MONOTONIC_CLOCK=OFF \
    -DLIBCXX_ENABLE_FILESYSTEM=OFF -DLIBCXX_ENABLE_RANDOM_DEVICE=OFF \
    -DLIBCXX_ENABLE_LOCALIZATION=OFF -DLIBCXX_ENABLE_WIDE_CHARACTERS=OFF \
    -DLIBCXX_ENABLE_TIME_ZONE_DATABASE=OFF -DLIBCXX_ENABLE_UNICODE=ON \
    -DLIBCXX_INCLUDE_TESTS=OFF -DLIBCXX_INCLUDE_BENCHMARKS=OFF \
    -DLIBCXX_TYPEINFO_COMPARISON_IMPLEMENTATION=2 -DLIBCXX_HARDENING_MODE=none

ninja -C "$BUILD" generate-cxx-headers
echo "headers: $BUILD/include/c++/v1"
