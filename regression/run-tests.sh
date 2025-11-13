#!/bin/bash

# SLOW-32 Regression Test Runner (Modern Version)
# Uses proper linking with libraries

set -e

# Paths
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"
LLVM_BIN="$HOME/llvm-project/build/bin"

# Tools
CLANG="$LLVM_BIN/clang"
LLC="$LLVM_BIN/llc"
ASSEMBLER="$SLOW32_BASE/tools/assembler/slow32asm"
LINKER="$SLOW32_BASE/tools/linker/s32-ld"
EMULATOR="$SLOW32_BASE/tools/emulator/slow32"

# Runtime components
CRT0="$SLOW32_BASE/runtime/crt0.s32o"
LIBC_DEBUG="$SLOW32_BASE/runtime/libc_debug.s32a"
LIBC_MMIO="$SLOW32_BASE/runtime/libc_mmio.s32a"
LIBS32="$SLOW32_BASE/runtime/libs32.s32a"

# Test configuration
TEST_DIR="$(dirname "$0")/tests"
RESULTS_DIR="$(dirname "$0")/results"
TIMEOUT=2

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Ensure runtime libraries are rebuilt with any local changes. [Let's not do this. It's out of control.]
# make -C "$SLOW32_BASE/runtime" libc_debug.s32a libs32.s32a >/dev/null

# Clean results directory
rm -rf "$RESULTS_DIR"
mkdir -p "$RESULTS_DIR"

echo "SLOW-32 Regression Tests (Modern Linker Version)"
echo "================================================="
echo ""

run_test() {
    local test_name="$1"
    local test_path="$TEST_DIR/$test_name"
    local result_path="$RESULTS_DIR/$test_name"
    
    TOTAL=$((TOTAL + 1))
    printf "%-30s " "$test_name:"
    
    if [ ! -f "$test_path/test.c" ]; then
        echo -e "${YELLOW}SKIP${NC} (no test.c)"
        return
    fi
    
    mkdir -p "$result_path"
    
    # Determine runtime variant
    local use_mmio=0
    if [ -f "$test_path/use_mmio" ]; then
        use_mmio=1
    fi

    # Optional command-line arguments
    local run_args=()
    if [ -f "$test_path/args.txt" ]; then
        mapfile -t run_args < "$test_path/args.txt"
    fi

    # Compile: C -> LLVM IR (with our runtime includes)
    if ! $CLANG -target slow32-unknown-none -S -emit-llvm -O0 \
         -I"$SLOW32_BASE/runtime/include" \
         "$test_path/test.c" -o "$result_path/test.ll" 2>"$result_path/compile.err"; then
        echo -e "${RED}FAIL${NC} (compile)"
        FAILED=$((FAILED + 1))
        return
    fi
    
    # Generate assembly: LLVM IR -> ASM
    if ! $LLC -mtriple=slow32-unknown-none \
         "$result_path/test.ll" -o "$result_path/test.s" 2>"$result_path/llc.err"; then
        echo -e "${RED}FAIL${NC} (llc)"
        FAILED=$((FAILED + 1))
        return
    fi
    
    # Assemble: ASM -> OBJ
    if ! $ASSEMBLER "$result_path/test.s" "$result_path/test.s32o" \
         >"$result_path/asm.out" 2>"$result_path/asm.err"; then
        echo -e "${RED}FAIL${NC} (assemble)"
        FAILED=$((FAILED + 1))
        return
    fi
    
    # Link: OBJ -> EXE (with proper libraries)
    local libc_archive="$LIBC_DEBUG"
    local linker_args=()
    if [ $use_mmio -eq 1 ]; then
        libc_archive="$LIBC_MMIO"
        linker_args+=(--mmio 64K)
    fi

    if ! $LINKER -o "$result_path/test.s32x" \
         "${linker_args[@]}" \
         $CRT0 "$result_path/test.s32o" "$libc_archive" $LIBS32 \
         2>"$result_path/link.err"; then
        echo -e "${RED}FAIL${NC} (link)"
        FAILED=$((FAILED + 1))
        return
    fi
    
    # Run with timeout
    if timeout $TIMEOUT $EMULATOR "$result_path/test.s32x" "${run_args[@]}" \
         >"$result_path/output_full.txt" 2>&1; then
        # Extract guest program output between "Starting execution" and the runtime summary
        awk '
            /^Starting execution/ { capture=1; next }
            /^HALT at PC/ { capture=0 }
            /^Program halted\./ { capture=0 }
            capture { print }
        ' "$result_path/output_full.txt" > "$result_path/output.txt"
        
        # Check expected output (strip trailing whitespace/newlines for comparison)
        if [ -f "$test_path/expected.txt" ]; then
            # Strip trailing whitespace and newlines for comparison
            tr -d '\n' < "$result_path/output.txt" > "$result_path/output_stripped.txt"
            tr -d '\n' < "$test_path/expected.txt" > "$result_path/expected_stripped.txt"
            
            if diff -q "$result_path/expected_stripped.txt" "$result_path/output_stripped.txt" >/dev/null 2>&1; then
                echo -e "${GREEN}PASS${NC}"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}FAIL${NC} (wrong output)"
                FAILED=$((FAILED + 1))
                # Show what we got for debugging
                echo "  Expected: $(cat $test_path/expected.txt)"
                echo "  Got:      $(cat $result_path/output.txt | tr -d '\n')"
            fi
        else
            # No expected output, just check it doesn't crash
            echo -e "${GREEN}PASS${NC} (no crash)"
            PASSED=$((PASSED + 1))
        fi
    else
        echo -e "${RED}FAIL${NC} (timeout/crash)"
        FAILED=$((FAILED + 1))
    fi
}

# Run all tests
for test in $(ls "$TEST_DIR" | sort); do
    run_test "$test"
done

echo ""
echo "================================================="
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed${NC}"
    exit 1
fi
