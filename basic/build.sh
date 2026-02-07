#!/bin/bash
set -e

# Paths (relative to basic/)
LLVM_BIN=~/llvm-project/build/bin
CLANG=$LLVM_BIN/clang
LLC=$LLVM_BIN/llc
ASM=../tools/assembler/slow32asm
LD=../tools/linker/s32-ld
TARGET=slow32-unknown-none
CFLAGS="-target $TARGET -S -emit-llvm -nostdinc -I../runtime/include"

# Optimization level (use -O0 for debugging, -O1 for production)
OPT_CORE="${OPT:--O0}"
OPT_SHELL="${OPT:--O0}"

echo "=== Building MY-BASIC for SLOW-32 ==="

# Step 1: Compile my_basic.c (core interpreter)
echo -n "  Compiling my_basic.c... "
$CLANG $CFLAGS $OPT_CORE -DNDEBUG my_basic.c -o my_basic.ll 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

echo -n "  Generating assembly (my_basic)... "
$LLC -mtriple=$TARGET my_basic.ll -o my_basic.s 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

echo -n "  Assembling my_basic... "
$ASM my_basic.s my_basic.s32o 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

# Step 2: Compile main.c (shell)
echo -n "  Compiling main.c... "
$CLANG $CFLAGS $OPT_SHELL -DNDEBUG main.c -o main.ll 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

echo -n "  Generating assembly (main)... "
$LLC -mtriple=$TARGET main.ll -o main.s 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

echo -n "  Assembling main... "
$ASM main.s main.s32o 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

# Step 3: Link with MMIO libc (for file I/O support)
echo -n "  Linking... "
$LD --mmio 64K -o basic.s32x \
    ../runtime/crt0.s32o \
    main.s32o \
    my_basic.s32o \
    ../runtime/libc_mmio.s32a \
    ../runtime/libs32.s32a 2>&1
if [ $? -ne 0 ]; then echo "FAILED"; exit 1; fi
echo "OK"

echo ""
echo "Success! Run with:"
echo "  ../tools/dbt/slow32-dbt basic.s32x"
echo "  or: ../tools/emulator/slow32-fast basic.s32x"
