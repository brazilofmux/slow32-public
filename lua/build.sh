#!/bin/bash
set -e

# Paths (relative to lua/)
LLVM_BIN=~/llvm-project/build/bin
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -fno-builtin -I../runtime/include -Isrc"

# Optimization level
OPT="${OPT:--O1}"

# Extra defines for Lua (LUA_USE_C89 is set in luaconf.h)
DEFINES=""

echo "=== Building Lua 5.4.7 for SLOW-32 ==="

# Compile all .c files in src/ except lua.c and luac.c (standalone entry points)
OBJECTS=""
for src in src/*.c; do
    base=$(basename "$src" .c)

    # Skip Lua's own main entry points
    if [ "$base" = "lua" ] || [ "$base" = "luac" ]; then
        continue
    fi

    echo -n "  Compiling $base.c... "
    $CLANG $CFLAGS $DEFINES $OPT "$src" -o "src/$base.ll" 2>&1
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
$LD --mmio 64K --stack-size 128K --data-size 5M -o lua.s32x \
    ../runtime/crt0.s32o \
    $OBJECTS \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a 2>&1
echo "OK"

echo ""
echo "Success! Run with:"
echo "  echo 'print(\"Hello from Lua!\")' | ../tools/emulator/slow32-fast lua.s32x"
