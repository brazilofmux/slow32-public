#!/bin/bash
# libc++ (LLVM's C++ standard library) and libc++abi (its C++ ABI library:
# exceptions, RTTI, exception_ptr, new/delete, the std exception classes)
# for SLOW-32, from the llvm-project tree the backend lives in -- the same
# LLVM, so no second pin to track.
#
#   scripts/build-libcxx.sh            configure, build, install
#
# Output, under $BUILD/install:
#   include/c++/v1   the header tree C++ programs compile against, ahead
#                    of runtime/include
#   lib/libc++.a     the compiled half of libc++ } s32-ar archives, linked
#   lib/libc++abi.a  libc++abi                   } before the C library:
#
#   clang++ -target slow32-unknown-none -nostdinc++ \
#       -isystem $BUILD/install/include/c++/v1 -isystem runtime/include \
#       -fexceptions -frtti -O2 -S -emit-llvm prog.cpp ...
#   s32-ld --mmio 64K runtime/crt0.s32o prog.s32o \
#       $BUILD/install/lib/libc++.a $BUILD/install/lib/libc++abi.a \
#       runtime/libc_mmio.s32a runtime/libs32.s32a
#
# Not with runtime/libs32cxx.s32a: that is the runtime's own C++ ABI, for
# freestanding C++ without libc++, and defines what libc++abi defines.
# libc++abi unwinds with the runtime's DWARF unwinder (unwind*.c in libc).
#
# Threads through the external thread API: SLOW-32 has one thread, and
# runtime/include/__external_threading says what that makes of a mutex, a
# condition variable and std::thread (which cannot start: EAGAIN).  The
# monotonic clock is GETTIME's (status 1).  No random device, no wide
# characters; filesystem and locales on; type_info compared by name.  Built at
# -O2, like the runtime, and archived with s32-ar (s32-ld reads its
# archives, not ar's) -- the archives keep libc++'s .a names.
# None of this is in git; the script regenerates it.
#
# The compilers, archiver and target come from scripts/slow32-toolchain.cmake.
#
# Env: LLVM_PROJECT (default ~/llvm-project), LLVM_BIN (default
# $LLVM_PROJECT/build/bin), BUILD (default $LLVM_PROJECT/build-slow32-libcxx).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LLVM_PROJECT="${LLVM_PROJECT:-$HOME/llvm-project}"
export LLVM_BIN="${LLVM_BIN:-$LLVM_PROJECT/build/bin}"
BUILD="${BUILD:-$LLVM_PROJECT/build-slow32-libcxx}"

# The clang driver assembles with slow32asm and links with s32-ld, by name.
export PATH="$ROOT/tools/assembler:$ROOT/tools/linker:$ROOT/tools/utilities:$PATH"

cmake -G Ninja -S "$LLVM_PROJECT/runtimes" -B "$BUILD" \
    -DCMAKE_TOOLCHAIN_FILE="$ROOT/scripts/slow32-toolchain.cmake" \
    -DLLVM_ENABLE_RUNTIMES="libcxx;libcxxabi" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS_RELEASE="-O2 -DNDEBUG" -DCMAKE_CXX_FLAGS_RELEASE="-O2 -DNDEBUG" \
    -DCMAKE_INSTALL_PREFIX="$BUILD/install" \
    -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_STATIC=ON -DLIBCXX_CXX_ABI=libcxxabi \
    -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_STATIC=ON \
    -DLIBCXXABI_USE_LLVM_UNWINDER=OFF \
    -DLIBCXXABI_ENABLE_THREADS=ON -DLIBCXXABI_HAS_EXTERNAL_THREAD_API=ON \
    -DLIBCXXABI_BAREMETAL=ON -DLIBCXXABI_INCLUDE_TESTS=OFF \
    -DLIBCXX_ENABLE_THREADS=ON -DLIBCXX_HAS_EXTERNAL_THREAD_API=ON \
    -DLIBCXX_ENABLE_MONOTONIC_CLOCK=ON \
    -DLIBCXX_ENABLE_FILESYSTEM=ON -DLIBCXX_ENABLE_RANDOM_DEVICE=OFF \
    -DLIBCXX_ENABLE_LOCALIZATION=ON -DLIBCXX_ENABLE_WIDE_CHARACTERS=OFF \
    -DLIBCXX_ENABLE_TIME_ZONE_DATABASE=OFF -DLIBCXX_ENABLE_UNICODE=ON \
    -DLIBCXX_INCLUDE_TESTS=OFF -DLIBCXX_INCLUDE_BENCHMARKS=OFF \
    -DLIBCXX_TYPEINFO_COMPARISON_IMPLEMENTATION=2 -DLIBCXX_HARDENING_MODE=none

ninja -C "$BUILD" install-cxx install-cxxabi
echo "headers: $BUILD/install/include/c++/v1"
echo "libraries: $BUILD/install/lib/libc++.a $BUILD/install/lib/libc++abi.a"
