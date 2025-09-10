#!/bin/bash
# analyze_switch.sh - Analyze how LLVM optimizes switch statements for SLOW-32
# Usage: ./analyze_switch.sh [source_file.c]

set -e

SOURCE="${1:-switch_patterns.c}"
BASE="${SOURCE%.c}"

echo "=== Analyzing Switch Statement Optimizations for $SOURCE ==="
echo

# Compile to LLVM IR
echo "1. Generating LLVM IR..."
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 "$SOURCE" -o "${BASE}.ll"

# Check for switch tables in IR
echo "2. Looking for switch table optimizations in LLVM IR..."
echo
grep -n "switch.table" "${BASE}.ll" || echo "No switch tables found in IR"
echo

# Generate assembly
echo "3. Generating SLOW-32 assembly..."
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none "${BASE}.ll" -o "${BASE}.s"

# Analyze assembly patterns
echo "4. Analyzing assembly patterns..."
echo
echo "Value tables (.word directives in .rodata):"
grep -A 5 "\.switch\.table" "${BASE}.s" 2>/dev/null || echo "  No value tables found"
echo
echo "Comparison cascades (seq instructions):"
grep -c "seq" "${BASE}.s" 2>/dev/null && echo "  Found $(grep -c 'seq' ${BASE}.s) comparison instructions" || echo "  No seq instructions found"
echo
echo "Bounds checks (sgtu/sleu for range testing):"
grep -E "sgtu|sleu" "${BASE}.s" | head -5 || echo "  No bounds checks found"
echo

# Build executable
echo "5. Building executable..."
../tools/assembler/slow32asm "${BASE}.s" "${BASE}.s32o"
../tools/linker/s32-ld -o "${BASE}.s32x" ../runtime/crt0.s32o "${BASE}.s32o" \
    ../runtime/libs32.s32a ../runtime/libc.s32a ../runtime/builtins.s32o

echo "6. Executable size analysis:"
ls -lh "${BASE}.s32x"

echo
echo "=== Analysis Complete ==="
echo
echo "Files generated:"
echo "  ${BASE}.ll   - LLVM IR (look for @switch.table globals)"
echo "  ${BASE}.s    - Assembly (look for .switch.table labels)"  
echo "  ${BASE}.s32x - Executable"
echo
echo "To see value tables: grep -A 10 'switch.table' ${BASE}.s"
echo "To run: ../tools/emulator/slow32 ${BASE}.s32x"