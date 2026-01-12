#!/bin/bash
# Build and run all C++ examples through the full SLOW32 toolchain

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SLOW32_ROOT="$SCRIPT_DIR/../.."
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"

CLANG="$LLVM_BIN/clang++"
LLC="$LLVM_BIN/llc"
ASM="$SLOW32_ROOT/tools/assembler/slow32asm"
LD="$SLOW32_ROOT/tools/linker/s32-ld"
EMU="$SLOW32_ROOT/tools/emulator/slow32"

RUNTIME="$SLOW32_ROOT/runtime"
CRT0="$RUNTIME/crt0.s32o"
LIBC="$RUNTIME/libc_debug.s32a"
LIBS32="$RUNTIME/libs32.s32a"

# C++ flags
CPP_FLAGS="-target slow32-unknown-none -fno-exceptions -fno-rtti -nostdinc++ -O1"

echo "=== SLOW32 C++ Build & Run Tests ==="
echo ""

passed=0
failed=0

for src in "$SCRIPT_DIR"/*.cpp; do
    name=$(basename "$src" .cpp)
    echo -n "[$name] "

    # Compile C++ -> LLVM IR
    ll_file="/tmp/${name}.ll"
    if ! "$CLANG" $CPP_FLAGS -S -emit-llvm "$src" -o "$ll_file" 2>/dev/null; then
        echo "FAIL (clang)"
        ((failed++))
        continue
    fi

    # LLVM IR -> Assembly
    s_file="/tmp/${name}.s"
    if ! "$LLC" -mtriple=slow32-unknown-none "$ll_file" -o "$s_file" 2>/dev/null; then
        echo "FAIL (llc)"
        ((failed++))
        continue
    fi

    # Assembly -> Object
    o_file="/tmp/${name}.s32o"
    if ! "$ASM" "$s_file" "$o_file" 2>/dev/null; then
        echo "FAIL (asm)"
        ((failed++))
        continue
    fi

    # Link
    x_file="/tmp/${name}.s32x"
    if ! "$LD" -o "$x_file" "$CRT0" "$o_file" "$LIBC" "$LIBS32" 2>/dev/null; then
        echo "FAIL (link)"
        ((failed++))
        continue
    fi

    # Run
    output=$("$EMU" "$x_file" 2>&1)
    if echo "$output" | grep -q "HALT"; then
        if [ "$name" = "11_global_destructor" ] && ! echo "$output" | grep -q "DTOR"; then
            echo "FAIL (no destructor output)"
            echo "  Output: $output"
            ((failed++))
        else
            cycles=$(echo "$output" | grep "Cycles:" | awk '{print $2}')
            echo "OK (${cycles} cycles)"
            ((passed++))
        fi
    else
        echo "FAIL (run)"
        echo "  Output: $output"
        ((failed++))
    fi
done

echo ""
echo "=== Results: $passed passed, $failed failed ==="

# Cleanup
rm -f /tmp/*.ll /tmp/*.s /tmp/*.s32o /tmp/*.s32x 2>/dev/null

exit $failed
