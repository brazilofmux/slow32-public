#!/bin/bash

# SLOW-32 Regression Test Runner (Modern Version)
# Uses proper linking with libraries

set -e

# Paths
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"

# Tool locations are env-overridable so the suite can be driven from the
# slow32:toolchain Docker image (or any other layout) without editing this
# script.  Defaults match a developer build with a local LLVM checkout.
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"

# Tools
CLANG="${CLANG:-$LLVM_BIN/clang}"
LLC="${LLC:-$LLVM_BIN/llc}"
ASSEMBLER="${ASSEMBLER:-$SLOW32_BASE/tools/assembler/slow32asm}"
LINKER="${LINKER:-$SLOW32_BASE/tools/linker/s32-ld}"
EMULATOR="${EMULATOR:-$SLOW32_BASE/tools/emulator/slow32}"
S32_CRT="${S32_CRT:-$SLOW32_BASE/tools/s32-crt/s32-crt}"

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
SKIPPED=0

# Tool availability (allow partial runs when LLVM tools are missing)
HAVE_CLANG=1
HAVE_LLC=1
if [ ! -x "$CLANG" ]; then
    HAVE_CLANG=0
fi
if [ ! -x "$LLC" ]; then
    HAVE_LLC=0
fi

# Ensure runtime libraries are rebuilt with any local changes. [Let's not do this. It's out of control.]
# make -C "$SLOW32_BASE/runtime" libc_debug.s32a libs32.s32a >/dev/null

# Clean results directory — but only for full-suite runs; a single-test
# invocation must not destroy the other tests' artifacts (run-differential.sh
# relies on them).
if [ $# -eq 0 ]; then
    rm -rf "$RESULTS_DIR"
fi
mkdir -p "$RESULTS_DIR"

echo "SLOW-32 Regression Tests (Modern Linker Version)"
echo "================================================="
echo ""
if [ $HAVE_CLANG -eq 0 ] || [ $HAVE_LLC -eq 0 ]; then
    echo "Note: LLVM tools not fully available."
    echo "  clang: $([ $HAVE_CLANG -eq 1 ] && echo yes || echo no)"
    echo "  llc:   $([ $HAVE_LLC -eq 1 ] && echo yes || echo no)"
    echo "C-based tests will be skipped; assembly-based tests will still run."
    echo ""
fi

run_test() {
    local test_name="$1"
    local test_path="$TEST_DIR/$test_name"
    local result_path="$RESULTS_DIR/$test_name"
    
    TOTAL=$((TOTAL + 1))
    printf "%-30s " "$test_name:"
    
    local asm_source=""
    if [ -f "$test_path/test.c" ]; then
        asm_source="c"
    elif [ -f "$test_path/test.s" ]; then
        asm_source="s"
    else
        # A test directory with no source is a broken test, not an absent
        # one: something committed the expectations and lost the case.
        # This used to be a SKIP, which is how four cases sat inert --
        # .gitignore's blanket "test*.s" swallowed their sources and the
        # runner reported nothing louder than a yellow line.
        echo -e "${RED}FAIL${NC} (no test.c or test.s -- source missing?)"
        FAILED=$((FAILED + 1))
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
        while IFS= read -r line || [ -n "$line" ]; do
            run_args+=("$line")
        done < "$test_path/args.txt"
    fi

    if [ "$asm_source" = "c" ]; then
        if [ $HAVE_CLANG -eq 0 ] || [ $HAVE_LLC -eq 0 ]; then
            echo -e "${YELLOW}SKIP${NC} (clang/llc unavailable)"
            SKIPPED=$((SKIPPED + 1))
            return
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
    else
        cp "$test_path/test.s" "$result_path/test.s"
    fi
    
    # Assemble: ASM -> OBJ
    if ! $ASSEMBLER "$result_path/test.s" "$result_path/test.s32o" \
         >"$result_path/asm.out" 2>"$result_path/asm.err"; then
        echo -e "${RED}FAIL${NC} (assemble)"
        FAILED=$((FAILED + 1))
        return
    fi

    # Compile and assemble any extra .c files (multi-file tests)
    local extra_objs=()
    for extra_c in "$test_path"/*.c; do
        [ -f "$extra_c" ] || continue
        [ "$(basename "$extra_c")" = "test.c" ] && continue
        local base
        base="$(basename "$extra_c" .c)"
        if ! $CLANG -target slow32-unknown-none -S -emit-llvm -O0 \
             -I"$SLOW32_BASE/runtime/include" \
             "$extra_c" -o "$result_path/$base.ll" 2>>"$result_path/compile.err"; then
            echo -e "${RED}FAIL${NC} (compile $base.c)"
            FAILED=$((FAILED + 1))
            return
        fi
        if ! $LLC -mtriple=slow32-unknown-none \
             "$result_path/$base.ll" -o "$result_path/$base.s" 2>>"$result_path/llc.err"; then
            echo -e "${RED}FAIL${NC} (llc $base.c)"
            FAILED=$((FAILED + 1))
            return
        fi
        if ! $ASSEMBLER "$result_path/$base.s" "$result_path/$base.s32o" \
             >>"$result_path/asm.out" 2>>"$result_path/asm.err"; then
            echo -e "${RED}FAIL${NC} (assemble $base.c)"
            FAILED=$((FAILED + 1))
            return
        fi
        extra_objs+=("$result_path/$base.s32o")
    done

    # Link: OBJ -> EXE (with proper libraries)
    local libc_archive="$LIBC_DEBUG"
    local linker_args=()
    if [ $use_mmio -eq 1 ]; then
        libc_archive="$LIBC_MMIO"
        linker_args+=(--mmio 64K)
    fi

    if ! $LINKER -o "$result_path/test.s32x" \
         "${linker_args[@]}" \
         $CRT0 "$result_path/test.s32o" "${extra_objs[@]}" "$libc_archive" $LIBS32 \
         2>"$result_path/link.err"; then
        echo -e "${RED}FAIL${NC} (link)"
        FAILED=$((FAILED + 1))
        return
    fi
    
    # Run with timeout.
    # The emulator propagates the guest exit code. We treat non-zero exits
    # as failures unless an expected_exit.txt is provided.
    # timeout returns 124 on timeout, 128+N on signal.
    local emu_exit=0
    local dump_dir=""
    local dump_env=()
    local viewer_pid=""
    local run_timeout=$TIMEOUT
    dump_env+=(S32_TUBE_PORT="$result_path/tube.port")
    if [ -f "$test_path/expected.hash" ]; then
        dump_dir="$result_path/tube"
        mkdir -p "$dump_dir"
        dump_env+=(S32_TUBE_DUMP="$dump_dir" S32_TUBE_DUMP_FULL=1)
    fi
    if [ -f "$test_path/inject.py" ]; then
        if ! command -v python3 >/dev/null 2>&1; then
            echo -e "${YELLOW}SKIP${NC} (python3 unavailable)"
            SKIPPED=$((SKIPPED + 1))
            return
        fi
        run_timeout=5
        rm -f "$result_path/tube.port"
        python3 "$test_path/inject.py" "$result_path/tube.port" \
            >"$result_path/inject.txt" 2>"$result_path/inject.err" &
        viewer_pid=$!
    elif [ -f "$test_path/viewer" ]; then
        if [ ! -x "$S32_CRT" ]; then
            echo -e "${YELLOW}SKIP${NC} (s32-crt not built)"
            SKIPPED=$((SKIPPED + 1))
            return
        fi
        # Executable is not the same as runnable: in the toolchain container
        # the host-built s32-crt is on the mount but its shared libraries are
        # not, and exec fails with "required file not found". That is a gap in
        # the environment, not a defect in the test.
        if ! "$S32_CRT" --help >/dev/null 2>&1; then
            echo -e "${YELLOW}SKIP${NC} (s32-crt present but not runnable here)"
            SKIPPED=$((SKIPPED + 1))
            return
        fi
        run_timeout=5
        rm -f "$result_path/tube.port"
        "$S32_CRT" --wait --once --text --port-file "$result_path/tube.port" \
            >"$result_path/viewer.txt" 2>"$result_path/viewer.err" &
        viewer_pid=$!
    fi
    if [ -f "$test_path/stdin.sh" ]; then
        # The test's stdin is a pipe fed by stdin.sh, which may pause before
        # it writes: an fd that is not readable yet, for wait-for-any tests.
        run_timeout=5
        env "${dump_env[@]}" timeout $run_timeout $EMULATOR "$result_path/test.s32x" "${run_args[@]}" \
             < <(bash "$test_path/stdin.sh") >"$result_path/output_full.txt" 2>&1 || emu_exit=$?
    else
        env "${dump_env[@]}" timeout $run_timeout $EMULATOR "$result_path/test.s32x" "${run_args[@]}" \
             >"$result_path/output_full.txt" 2>&1 || emu_exit=$?
    fi
    if [ -n "$viewer_pid" ]; then
        wait "$viewer_pid" 2>/dev/null || true
        viewer_pid=""
    fi

    if [ $emu_exit -ge 124 ]; then
        echo -e "${RED}FAIL${NC} (timeout/crash, exit=$emu_exit)"
        FAILED=$((FAILED + 1))
    else
        # Validate exit code if needed
        local expected_exit_file="$test_path/expected_exit.txt"
        if [ -f "$expected_exit_file" ]; then
            local expected_exit
            expected_exit=$(tr -d ' \t\n' < "$expected_exit_file")
            if [ -z "$expected_exit" ]; then
                expected_exit=0
            fi
            if [ "$emu_exit" != "$expected_exit" ]; then
                echo -e "${RED}FAIL${NC} (exit=$emu_exit, expected=$expected_exit)"
                FAILED=$((FAILED + 1))
                return
            fi
        else
            if [ "$emu_exit" != "0" ]; then
                echo -e "${RED}FAIL${NC} (exit=$emu_exit)"
                FAILED=$((FAILED + 1))
                return
            fi
        fi

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
                if [ -f "$test_path/expected.hash" ]; then
                    if [ ! -f "$dump_dir/000000.hash" ]; then
                        echo -e "${RED}FAIL${NC} (no tube dump)"
                        FAILED=$((FAILED + 1))
                        return
                    fi
                    if ! diff -q "$test_path/expected.hash" "$dump_dir/000000.hash" >/dev/null 2>&1; then
                        echo -e "${RED}FAIL${NC} (tube hash)"
                        FAILED=$((FAILED + 1))
                        echo "  Expected: $(tr -d '\n' < "$test_path/expected.hash")"
                        echo "  Got:      $(tr -d '\n' < "$dump_dir/000000.hash")"
                        return
                    fi
                fi
                if [ -f "$test_path/expected.vseg.txt" ]; then
                    tr -d '\r' < "$result_path/viewer.txt" > "$result_path/viewer_stripped.txt"
                    tr -d '\r' < "$test_path/expected.vseg.txt" > "$result_path/vseg_expected_stripped.txt"
                    if ! diff -q "$result_path/vseg_expected_stripped.txt" "$result_path/viewer_stripped.txt" >/dev/null 2>&1; then
                        echo -e "${RED}FAIL${NC} (vseg)"
                        FAILED=$((FAILED + 1))
                        echo "  Expected: $(tr '\n' ' ' < "$test_path/expected.vseg.txt")"
                        echo "  Got:      $(tr '\n' ' ' < "$result_path/viewer.txt")"
                        return
                    fi
                fi
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
    fi
}

# Run all tests, or just the ones named on the command line
if [ $# -gt 0 ]; then
    for test in "$@"; do
        run_test "$test"
    done
else
    for test in $(ls "$TEST_DIR" | sort); do
        run_test "$test"
    done
fi

echo ""
echo "================================================="
echo "Results: $PASSED passed, $FAILED failed, $SKIPPED skipped (of $TOTAL)"
echo ""

if [ $FAILED -eq 0 ] && [ $SKIPPED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
elif [ $FAILED -eq 0 ]; then
    # Skips are legitimate here (a box without clang/llc runs only the
    # assembly cases), but they are not passes -- saying "All tests
    # passed" over a run that was mostly skipped is how a silently
    # inert case survives.
    echo -e "${GREEN}$PASSED passed${NC}, $SKIPPED skipped, none failed"
    exit 0
else
    echo -e "${RED}Some tests failed${NC}"
    exit 1
fi
