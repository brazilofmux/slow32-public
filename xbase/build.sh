#!/bin/bash
set -e

# Paths (relative to xbase/); an installed toolchain (the slow32:xbase
# image's builder) names its own: LLVM_BIN, S32_AS, S32_LD, S32_RT
# (crt0.s32o and the libraries), S32_RT_INCLUDE
cd "$(dirname "$0")"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM="${S32_AS:-../tools/assembler/slow32asm}"
LD="${S32_LD:-../tools/linker/s32-ld}"
RT="${S32_RT:-../runtime}"
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I${S32_RT_INCLUDE:-../runtime/include}"

# Optimization level
OPT="${OPT:--O1}"

echo "=== Building SLOW-32 xBase (the interpreter) ==="

# Auto-discover all .c files in src/
OBJECTS=""
for src in src/*.c; do
    base=$(basename "$src" .c)
    echo -n "  Compiling $base.c... "
    $CLANG $CFLAGS $OPT -DNDEBUG "$src" -o "src/$base.ll" 2>&1
    echo "OK"

    echo -n "  Generating assembly ($base)... "
    $LLC -mtriple=$TARGET "src/$base.ll" -o "src/$base.s" 2>&1
    echo "OK"

    echo -n "  Assembling $base... "
    $ASM "src/$base.s" "src/$base.s32o" 2>&1
    echo "OK"

    OBJECTS="$OBJECTS src/$base.s32o"
done

# Link with MMIO libc
echo -n "  Linking... "
$LD --mmio 64K --stack-size 128K --data-size 8M --heap-size 64M -o xbase.s32x \
    "$RT/crt0.s32o" \
    $OBJECTS \
    "$RT/libc_mmio.s32a" \
    "$RT/libs32.s32a" 2>&1
echo "OK"

echo ""
echo "Success! Run with:"
echo "  echo 'QUIT' | ../tools/emulator/slow32-fast xbase.s32x"
