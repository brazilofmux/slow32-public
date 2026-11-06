#!/bin/sh
# Inside an Alpine container (slow32:toolchain), source at /src read-only, build at /b: flang
# on Linux, where int64_t and long are one type (docs/plans/flang.md):
#   podman run --rm -v ~/llvm-project:/src:ro -v flang-linux:/b IMAGE sh /src-script
set -e
apk add -q cmake samurai python3 linux-headers
cmake -S /src/llvm -B /b -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ \
  -DLLVM_ENABLE_PROJECTS="clang;mlir;flang" -DLLVM_TARGETS_TO_BUILD=SLOW32 -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=SLOW32 \
  -DLLVM_DEFAULT_TARGET_TRIPLE=slow32-unknown-none -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_EXAMPLES=OFF -DMLIR_ENABLE_EXECUTION_ENGINE=OFF -DFLANG_INCLUDE_TESTS=OFF -DCLANG_INCLUDE_TESTS=OFF \
  -DLLVM_ENABLE_ZSTD=OFF -DLLVM_ENABLE_ZLIB=OFF >/dev/null
ninja -C /b -j2 flang llc   # g++ on flang runs a 24 GB VM out of memory at -j4
/b/bin/flang --version | head -1
