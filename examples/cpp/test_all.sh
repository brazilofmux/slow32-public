#!/bin/bash
# Test all C++ examples through the SLOW32 toolchain

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
RUNTIME_INC="$SCRIPT_DIR/../../runtime/include"

CLANG="$LLVM_BIN/clang++"
LLC="$LLVM_BIN/llc"

# Standard flags for embedded C++ without exceptions/RTTI
# Note: Don't use -ffreestanding as it mangles main()
CPP_FLAGS="-target slow32-unknown-none -fno-exceptions -fno-rtti -nostdinc++ -O1"

echo "=== SLOW32 C++ Compilation Tests ==="
echo "Using clang: $CLANG"
echo "Using llc: $LLC"
echo ""

# Test each file
for src in "$SCRIPT_DIR"/*.cpp; do
    name=$(basename "$src" .cpp)
    echo "--- Testing: $name ---"

    # Step 1: C++ to LLVM IR
    ll_file="$SCRIPT_DIR/$name.ll"
    echo -n "  [1] Clang (C++ -> LLVM IR): "
    if "$CLANG" $CPP_FLAGS -S -emit-llvm "$src" -o "$ll_file" 2>&1; then
        echo "OK"
    else
        echo "FAILED"
        continue
    fi

    # Step 2: LLVM IR to SLOW32 Assembly
    s_file="$SCRIPT_DIR/$name.s"
    echo -n "  [2] LLC (LLVM IR -> ASM): "
    if "$LLC" -mtriple=slow32-unknown-none "$ll_file" -o "$s_file" 2>&1; then
        echo "OK"
    else
        echo "FAILED"
        continue
    fi

    # Step 3: Check for undefined symbols that would need runtime support
    echo -n "  [3] Checking for C++ runtime symbols: "
    cxx_symbols=$(grep -E '__cxa_|__gxx_|operator (new|delete)|__dso_handle' "$s_file" 2>/dev/null | head -5)
    if [ -n "$cxx_symbols" ]; then
        echo ""
        echo "$cxx_symbols" | sed 's/^/      /'
    else
        echo "none found"
    fi

    echo ""
done

echo "=== Summary ==="
echo "Check .ll and .s files for details"
