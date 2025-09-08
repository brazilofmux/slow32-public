#!/bin/bash

# Test runner for SLOW32 backend verification suite
# Run after build completes to verify all claimed features still work

LLVM_BUILD="/ztank/secret/sdennis/llvm-project/build"
LLC="$LLVM_BUILD/bin/llc"
CLANG="$LLVM_BUILD/bin/clang"
SLOW32_ROOT="/ztank/secret/sdennis/slow-32"
ASM="$SLOW32_ROOT/assembler/slow32asm"
LINKER="$SLOW32_ROOT/linker/s32-ld"
EMU="$SLOW32_ROOT/emulator/slow32"
RUNTIME="$SLOW32_ROOT/runtime"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
PASSED=0
FAILED=0
SKIPPED=0

# Function to run a single test
run_test() {
    local test_name=$1
    local test_file=$2
    
    echo -n "Testing $test_name... "
    
    # Check if LLVM IR file exists
    if [ ! -f "$test_file" ]; then
        echo -e "${YELLOW}SKIPPED${NC} (file not found)"
        ((SKIPPED++))
        return
    fi
    
    # Compile LLVM IR to SLOW32 assembly
    if ! $LLC -mtriple=slow32-unknown-none "$test_file" -o "${test_file%.ll}.s" 2>/dev/null; then
        echo -e "${RED}FAILED${NC} (llc error)"
        ((FAILED++))
        return
    fi
    
    # Assemble (slow32asm uses positional args, not -o flag)
    if ! $ASM "${test_file%.ll}.s" "${test_file%.ll}.o" 2>/dev/null; then
        echo -e "${RED}FAILED${NC} (assembler error)"
        ((FAILED++))
        return
    fi
    
    # Link (include debug_char.s32o for output support)
    if ! $LINKER -o "${test_file%.ll}.s32x" \
         "$RUNTIME/crt0.s32o" \
         "${test_file%.ll}.o" \
         "$RUNTIME/intrinsics.s32o" \
         "$RUNTIME/debug_char.s32o" 2>/dev/null; then
        echo -e "${RED}FAILED${NC} (linker error)"
        ((FAILED++))
        return
    fi
    
    # Run and capture output
    OUTPUT=$($EMU "${test_file%.ll}.s32x" 2>&1)
    EXITCODE=$?
    
    # Check for expected output
    if [[ "$OUTPUT" == *"PASS"* ]]; then
        echo -e "${GREEN}PASSED${NC}"
        ((PASSED++))
    elif [[ "$OUTPUT" == *"OK"* ]]; then
        echo -e "${GREEN}PASSED${NC}"
        ((PASSED++))
    else
        echo -e "${RED}FAILED${NC} (output: $OUTPUT)"
        ((FAILED++))
    fi
    
    # Clean up intermediate files
    rm -f "${test_file%.ll}.s" "${test_file%.ll}.o" "${test_file%.ll}.s32x"
}

# Main test execution
echo "================================"
echo "SLOW32 Backend Verification Suite"
echo "================================"
echo

# Check if build is ready
if [ ! -x "$LLC" ]; then
    echo -e "${RED}Error: llc not found or not executable${NC}"
    echo "Please wait for build to complete"
    exit 1
fi

# Run all tests
echo "Running feature tests..."
echo

run_test "Arithmetic operations" "test_arithmetic.ll"
run_test "Shift operations" "test_shifts.ll"
run_test "Function calls" "test_function_calls.ll"
run_test "Global variables" "test_globals.ll"
run_test "Varargs support" "test_varargs.ll"
run_test "Branch instructions" "test_branches.ll"
run_test "Edge cases" "test_edge_cases.ll"
run_test "Stack stress" "test_stack_stress.ll"
run_test "Immediate patterns" "test_immediate_patterns.ll"

echo
echo "================================"
echo "Test Results Summary"
echo "================================"
echo -e "Passed:  ${GREEN}$PASSED${NC}"
echo -e "Failed:  ${RED}$FAILED${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
echo

if [ $FAILED -eq 0 ] && [ $PASSED -gt 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
elif [ $FAILED -gt 0 ]; then
    echo -e "${RED}Some tests failed${NC}"
    exit 1
else
    echo -e "${YELLOW}No tests were run${NC}"
    exit 2
fi