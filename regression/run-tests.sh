#!/bin/bash

# SLOW32 Regression Test Runner
# Runs all tests in the tests/ directory and reports results

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/config.sh"

# Statistics
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

# Results directory
RESULTS_DIR="$SCRIPT_DIR/results"
rm -rf "$RESULTS_DIR"
mkdir -p "$RESULTS_DIR"

# Log file
LOG_FILE="$RESULTS_DIR/test-run-$(date +%Y%m%d-%H%M%S).log"

log() {
    echo "$@" | tee -a "$LOG_FILE"
}

run_test() {
    local test_dir="$1"
    local test_name="$(basename "$test_dir")"
    local result_dir="$RESULTS_DIR/$test_name"
    
    mkdir -p "$result_dir"
    
    TOTAL=$((TOTAL + 1))
    
    # Check for required files - either test.c or test.ll
    local test_source=""
    local test_type=""
    if [ -f "$test_dir/test.c" ]; then
        test_source="$test_dir/test.c"
        test_type="c"
    elif [ -f "$test_dir/test.ll" ]; then
        test_source="$test_dir/test.ll"
        test_type="ll"
    else
        log "  ${YELLOW}SKIP${NC}: No test.c or test.ll found"
        SKIPPED=$((SKIPPED + 1))
        return
    fi
    
    # Check for skip marker
    if [ -f "$test_dir/SKIP" ]; then
        log "  ${YELLOW}SKIP${NC}: $(cat "$test_dir/SKIP" 2>/dev/null || echo "Marked to skip")"
        SKIPPED=$((SKIPPED + 1))
        return
    fi
    
    # Compilation pipeline
    local error_stage=""
    
    # Handle different test types
    if [ "$test_type" = "c" ]; then
        # 1. Compile C to LLVM IR (check for optimization level file)
        local opt_level="-O0"  # Default to no optimization
        if [ -f "$test_dir/optimize" ]; then
            opt_level="-O$(cat "$test_dir/optimize")"
        fi
        
        if ! $CLANG -target slow32-unknown-none -S -emit-llvm $opt_level \
             -ffreestanding -nostdlibinc \
             "$test_source" -o "$result_dir/test.ll" 2>"$result_dir/clang.err"; then
            error_stage="Clang compilation"
        # 2. Compile LLVM IR to SLOW32 assembly using LLC
        elif ! $LLC -mtriple=slow32-unknown-none \
               "$result_dir/test.ll" -o "$result_dir/test.s" 2>"$result_dir/llc.err"; then
            error_stage="LLC compilation"
        fi
    else
        # For .ll files, skip C compilation and go straight to LLC
        if ! cp "$test_source" "$result_dir/test.ll"; then
            error_stage="Copy LLVM IR"
        elif ! $LLC -mtriple=slow32-unknown-none \
               "$result_dir/test.ll" -o "$result_dir/test.s" 2>"$result_dir/llc.err"; then
            error_stage="LLC compilation"
        fi
    fi
    
    # Continue with assembly and linking if no errors yet
    if [ -z "$error_stage" ]; then
        # 3. Assemble
        if ! $SLOW32_ASM "$result_dir/test.s" "$result_dir/test.s32o" 2>"$result_dir/asm.err"; then
            error_stage="Assembly"
        # 4. Link - check if we need builtins for 64-bit operations
        elif [[ "$test_name" == *"i64"* ]]; then
            # Include builtins for 64-bit tests
            if ! $SLOW32_LD -o "$result_dir/test.s32x" \
                   $CRT0 "$result_dir/test.s32o" $DEBUG_CHAR $INTRINSICS $BUILTINS 2>"$result_dir/link.err"; then
                error_stage="Linking"
            fi
        elif ! $SLOW32_LD -o "$result_dir/test.s32x" \
               $CRT0 "$result_dir/test.s32o" $DEBUG_CHAR $INTRINSICS 2>"$result_dir/link.err"; then
            error_stage="Linking"
        fi
    fi
    
    if [ -n "$error_stage" ]; then
        log "  ${RED}FAIL${NC}: $error_stage failed"
        [ $VERBOSE -eq 1 ] && cat "$result_dir"/*.err 2>/dev/null
        FAILED=$((FAILED + 1))
        return
    fi
    
    # 5. Run with timeout (capture full output, then extract debug_char lines)
    timeout $TEST_TIMEOUT $SLOW32_EMU "$result_dir/test.s32x" \
        >"$result_dir/full_output.txt" 2>"$result_dir/runtime.err"
    local exit_code=$?
    
    # Extract just the debug_char output (between "Starting execution" and "HALT at PC")
    sed -n '/^Starting execution/,/^HALT at PC/{/^Starting execution/d;/^HALT at PC/d;p}' \
        "$result_dir/full_output.txt" | tr -d '\n' > "$result_dir/output.txt"
    
    # Check result
    if [ $exit_code -eq 124 ]; then
        log "  ${RED}FAIL${NC}: Timeout (likely infinite loop)"
        FAILED=$((FAILED + 1))
    elif [ $exit_code -ne 0 ] && [ ! -f "$test_dir/expect-fail" ]; then
        log "  ${RED}FAIL${NC}: Runtime error (exit code: $exit_code)"
        FAILED=$((FAILED + 1))
    elif [ -f "$test_dir/expected.txt" ]; then
        # Compare output
        if diff -q "$test_dir/expected.txt" "$result_dir/output.txt" >/dev/null 2>&1; then
            log "  ${GREEN}PASS${NC}"
            PASSED=$((PASSED + 1))
        else
            log "  ${RED}FAIL${NC}: Output mismatch"
            [ $VERBOSE -eq 1 ] && diff "$test_dir/expected.txt" "$result_dir/output.txt"
            FAILED=$((FAILED + 1))
        fi
    elif [ -f "$test_dir/expect-fail" ]; then
        # Test is expected to fail
        if [ $exit_code -eq 0 ]; then
            log "  ${RED}FAIL${NC}: Expected to fail but passed"
            FAILED=$((FAILED + 1))
        else
            log "  ${GREEN}PASS${NC} (correctly failed)"
            PASSED=$((PASSED + 1))
        fi
    else
        # No expected output, just check it ran
        if [ $exit_code -eq 0 ] || [ $exit_code -eq 124 ]; then
            log "  ${GREEN}PASS${NC} (no crash)"
            PASSED=$((PASSED + 1))
        else
            log "  ${RED}FAIL${NC}: Crashed"
            FAILED=$((FAILED + 1))
        fi
    fi
}

# Main test loop
log "SLOW32 Regression Test Suite"
log "============================="
log "Started at: $(date)"
log ""

# Check for specific test argument
if [ $# -gt 0 ]; then
    # Run specific tests
    for test_pattern in "$@"; do
        for test_dir in $SCRIPT_DIR/tests/*"$test_pattern"*; do
            if [ -d "$test_dir" ]; then
                test_name="$(basename "$test_dir")"
                log "Running: $test_name"
                run_test "$test_dir"
            fi
        done
    done
else
    # Run all tests
    for test_dir in $SCRIPT_DIR/tests/*/; do
        if [ -d "$test_dir" ]; then
            test_name="$(basename "$test_dir")"
            log "Running: $test_name"
            run_test "$test_dir"
        fi
    done
fi

# Summary
log ""
log "============================="
log "Results:"
log "  Total:   $TOTAL"
log "  ${GREEN}Passed:  $PASSED${NC}"
log "  ${RED}Failed:  $FAILED${NC}"
log "  ${YELLOW}Skipped: $SKIPPED${NC}"
log ""

if [ $FAILED -eq 0 ] && [ $PASSED -gt 0 ]; then
    log "${GREEN}All tests passed!${NC}"
    exit 0
elif [ $FAILED -gt 0 ]; then
    log "${RED}Some tests failed.${NC}"
    log "Check $LOG_FILE for details"
    exit 1
else
    log "${YELLOW}No tests were run successfully.${NC}"
    exit 1
fi