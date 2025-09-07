#!/bin/bash
# Compile C programs for SLOW32 with full runtime support

if [ $# -lt 1 ]; then
    echo "Usage: $0 <source.c> [output.s32x]"
    echo "Compiles a C program to SLOW32 executable with runtime support"
    exit 1
fi

SOURCE="$1"
BASENAME="${SOURCE%.c}"
OUTPUT="${2:-$BASENAME.s32x}"

# Paths
LLVM_BIN="$HOME/llvm-project/build/bin"
SLOW32_ROOT="$HOME/slow-32"

# Check source exists
if [ ! -f "$SOURCE" ]; then
    echo "Error: Source file '$SOURCE' not found"
    exit 1
fi

echo "Compiling $SOURCE to $OUTPUT..."

# Step 1: C to LLVM IR
echo "  [1/4] Generating LLVM IR..."
$LLVM_BIN/clang -target slow32-unknown-none -S -emit-llvm -O1 "$SOURCE" -o "$BASENAME.ll"
if [ $? -ne 0 ]; then
    echo "Error: Failed to generate LLVM IR"
    exit 1
fi

# Step 2: LLVM IR to SLOW32 assembly
echo "  [2/4] Generating SLOW32 assembly..."
$LLVM_BIN/llc -mtriple=slow32-unknown-none "$BASENAME.ll" -o "$BASENAME.s"
if [ $? -ne 0 ]; then
    echo "Error: Failed to generate assembly"
    exit 1
fi

# Step 3: Assembly to object
echo "  [3/4] Assembling..."
$SLOW32_ROOT/assembler/slow32asm "$BASENAME.s" "$BASENAME.s32o"
if [ $? -ne 0 ]; then
    echo "Error: Failed to assemble"
    exit 1
fi

# Step 4: Link with runtime
echo "  [4/4] Linking with runtime..."
RUNTIME_OBJS="$SLOW32_ROOT/runtime/crt0.s32o"
RUNTIME_OBJS="$RUNTIME_OBJS $SLOW32_ROOT/runtime/intrinsics.s32o"
RUNTIME_OBJS="$RUNTIME_OBJS $SLOW32_ROOT/runtime/stdlib.s32o"
RUNTIME_OBJS="$RUNTIME_OBJS $SLOW32_ROOT/runtime/builtins.s32o"
RUNTIME_OBJS="$RUNTIME_OBJS $SLOW32_ROOT/runtime/debug_char.s32o"

# Add printf if needed
if grep -q "printf" "$BASENAME.s"; then
    RUNTIME_OBJS="$RUNTIME_OBJS $SLOW32_ROOT/runtime/printf.s32o"
fi

$SLOW32_ROOT/linker/s32-ld -o "$OUTPUT" $RUNTIME_OBJS "$BASENAME.s32o"
if [ $? -ne 0 ]; then
    echo "Error: Failed to link"
    exit 1
fi

echo "Success! Created $OUTPUT"
echo "Run with: $SLOW32_ROOT/emulator/slow32 $OUTPUT"