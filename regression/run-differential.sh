#!/bin/bash

# SLOW-32 N-way differential harness
#
# Runs every regression test binary under all available execution engines and
# diffs their outputs against the reference interpreter. This is the guard
# against semantic drift between the independently hand-written engines
# (slow32, slow32-fast, slow32-dbt, qemu-system-slow32).
#
# Engines:
#   slow32       reference interpreter      output + exit code compared
#   slow32-fast  predecoded interpreter     output + exit code compared
#   slow32-dbt   dynamic binary translator  output + exit code compared
#   qemu         qemu-system-slow32 (TCG)   output only (QEMU does not
#                                           propagate guest exit codes)
#
# Missing engines are reported as loud SKIPs, never silently dropped.
# Tests with args.txt are skipped under QEMU (-kernel has no guest argv).
#
# Usage: ./run-differential.sh [test-name ...]
#   With no arguments, runs every test in tests/.
#   Test binaries are taken from results/<test>/test.s32x; missing ones are
#   built via ./run-tests.sh <test>. REBUILD=1 forces a rebuild.
#
# Env overrides: SLOW32, SLOW32_FAST, SLOW32_DBT, QEMU_S32, TIMEOUT (secs).
#
# Exit codes: 0 all engines agree, 1 divergence found, 2 setup error.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"

SLOW32="${SLOW32:-$SLOW32_BASE/tools/emulator/slow32}"
SLOW32_FAST="${SLOW32_FAST:-$SLOW32_BASE/tools/emulator/slow32-fast}"
SLOW32_DBT="${SLOW32_DBT:-$SLOW32_BASE/tools/dbt/slow32-dbt}"
QEMU_S32="${QEMU_S32:-$HOME/qemu/build/qemu-system-slow32}"

TEST_DIR="$SCRIPT_DIR/tests"
RESULTS_DIR="$SCRIPT_DIR/results"
TIMEOUT="${TIMEOUT:-10}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

if [ ! -x "$SLOW32" ]; then
    echo "ERROR: reference interpreter not found: $SLOW32" >&2
    exit 2
fi

# Engine roster: name|path|compare-exit-code
ENGINES=()
SKIPPED_ENGINES=()
for spec in "slow32-fast|$SLOW32_FAST|1" \
            "slow32-dbt|$SLOW32_DBT|1" \
            "qemu|$QEMU_S32|0"; do
    name="${spec%%|*}"
    path="$(echo "$spec" | cut -d'|' -f2)"
    if [ -x "$path" ]; then
        ENGINES+=("$spec")
    else
        SKIPPED_ENGINES+=("$name ($path)")
    fi
done

if [ ${#ENGINES[@]} -eq 0 ]; then
    echo "ERROR: no engines to compare against the reference" >&2
    exit 2
fi

echo "Reference: $SLOW32"
echo "Engines under test:"
for spec in "${ENGINES[@]}"; do
    echo "  ${spec%%|*}: $(echo "$spec" | cut -d'|' -f2)"
done
if [ ${#SKIPPED_ENGINES[@]} -gt 0 ]; then
    for s in "${SKIPPED_ENGINES[@]}"; do
        echo -e "  ${YELLOW}SKIPPED ENGINE${NC}: $s -- not built/available on this host"
    done
fi
echo "(not runnable on this host: s32fast-hir / cross-compiled emulators are Linux ELF)"
echo ""

# Strip per-engine banners and stats so only guest output remains.
# Memory-fault reports are canonicalized to "FAULT addr=0x..." so the same
# fault with different wording agrees, while a missing fault or a different
# fault address still diverges. Reported PCs are dropped: engines with native
# intrinsic stubs (dbt, qemu) have no guest PC at the fault point.
normalize_output() {
    sed \
        -e 's/\r$//' \
        -e 's/^Memory fault: Failed to .* bytes at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^Error: .* out of bounds at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^Error: .* out of bounds or to protected memory at \(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e 's/^DBT: Memory fault at PC=[0-9xA-Fa-f]*, addr=\(0x[0-9A-Fa-f]*\).*/FAULT addr=\1/' \
        -e '/^Starting execution/d' \
        -e '/^MMIO enabled/d' \
        -e '/^HALT at/d' \
        -e '/^Program halted/d' \
        -e '/^Exit code:/d' \
        -e '/^Instructions executed/d' \
        -e '/^Simulated cycles:/d' \
        -e '/^Wall time:/d' \
        -e '/^Performance:/d' \
        -e '/^Cycles:/d' \
        -e '/instructions\/second/d' \
        -e '/^slow32: native intrinsics detected:/d' \
        -e '/^  mem[a-z]*:  *0x[0-9a-f]*$/d' \
        -e '/^  strlen:  *0x[0-9a-f]*$/d' \
    | awk '
        # Engines print fault addresses in mixed hex case; fold canonicalized
        # fault lines to lowercase so 0x2000000F and 0x2000000f agree.
        /^FAULT addr=/ { $0 = tolower($0) }
        { lines[NR] = $0; if ($0 ~ /[^[:space:]]/) last = NR }
        END { for (i = 1; i <= last; i++) print lines[i] }
    '
}

TOTAL=0
AGREE=0
DIVERGED=0
SKIPPED=0
DIVERGED_TESTS=()

# Divergences that are understood and accepted. These four disagree only when
# qemu is in the comparison: qemu and the DBT dispatch memcpy/memset/strlen to
# native host stubs, so an out-of-bounds access faults at a different
# granularity (and without a guest PC) than the interpreter walking bytes. The
# same four, and only these four, diverge identically on arm64 and x86-64, and
# all four agree when qemu is dropped — confirming host-arch and DBT are not
# involved.
#
# Only honored under ALLOW_KNOWN_DIVERGENCES=1 (CI sets it) so that an
# interactive run still reports them. A divergence outside this list always
# fails, so a new bug cannot hide behind the allowlist.
# History: bug-dbt-intrinsic-bounds* used to diverge on TWO engines. The DBT
# half (A64 stubs stored EXIT_REASON into exit_info when info_reg was W0) was
# fixed in translate_a64.c emit_a64_stub_fault_exit (Pack B, 2026-08) — the
# DBT now matches the reference exactly, verified 2026-08-08. The remaining
# divergence is qemu-only: qemu-system-slow32 exits silently on an
# out-of-bounds intrinsic access where the reference prints
# "fault addr=...". Still open (AUDIT-2026-08 "QEMU fault reporting"),
# re-verified against a qemu built from bce30bac2c. When that lands, empty
# this list again — and re-run this harness before believing it.
KNOWN_DIVERGENT="bug-dbt-intrinsic-bounds
bug-dbt-intrinsic-bounds-memcpy
bug-dbt-intrinsic-bounds-memset
bug-dbt-intrinsic-bounds-strlen"

run_engine() {
    # $1 engine name, $2 engine path, $3 s32x, $4 out-file, then guest args
    local name="$1" path="$2" s32x="$3" out="$4"
    shift 4
    local rc=0
    if [ "$name" = "qemu" ]; then
        timeout "$TIMEOUT" "$path" -machine slow32-tcg -nographic -monitor none \
            -kernel "$s32x" </dev/null >"$out" 2>&1 || rc=$?
    else
        timeout "$TIMEOUT" "$path" "$s32x" "$@" </dev/null >"$out" 2>&1 || rc=$?
    fi
    echo "$rc"
}

run_test() {
    local test_name="$1"
    local test_path="$TEST_DIR/$test_name"
    local result_path="$RESULTS_DIR/$test_name"
    local s32x="$result_path/test.s32x"
    local diff_dir="$result_path/diff"

    TOTAL=$((TOTAL + 1))
    printf "%-34s " "$test_name:"

    if [ ! -f "$test_path/test.c" ] && [ ! -f "$test_path/test.s" ]; then
        echo -e "${YELLOW}SKIP${NC} (no test source)"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    if [ ! -f "$s32x" ] || [ -n "$REBUILD" ]; then
        (cd "$SCRIPT_DIR" && ./run-tests.sh "$test_name" >/dev/null 2>&1)
        if [ ! -f "$s32x" ]; then
            echo -e "${YELLOW}SKIP${NC} (no test.s32x; build failed?)"
            SKIPPED=$((SKIPPED + 1))
            return
        fi
    fi

    local run_args=()
    if [ -f "$test_path/args.txt" ]; then
        while IFS= read -r line || [ -n "$line" ]; do
            run_args+=("$line")
        done < "$test_path/args.txt"
    fi

    mkdir -p "$diff_dir"

    local ref_rc
    ref_rc=$(run_engine ref "$SLOW32" "$s32x" "$diff_dir/ref.out" "${run_args[@]}")
    normalize_output < "$diff_dir/ref.out" > "$diff_dir/ref.norm"
    if [ "$ref_rc" -ge 124 ]; then
        echo -e "${YELLOW}SKIP${NC} (reference timeout/crash, rc=$ref_rc)"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    local verdict="agree"
    local details=""
    for spec in "${ENGINES[@]}"; do
        local name path cmp_rc
        name="${spec%%|*}"
        path="$(echo "$spec" | cut -d'|' -f2)"
        cmp_rc="$(echo "$spec" | cut -d'|' -f3)"

        if [ "$name" = "qemu" ] && [ ${#run_args[@]} -gt 0 ]; then
            details="$details [qemu: skipped, guest argv unsupported]"
            continue
        fi

        local rc
        rc=$(run_engine "$name" "$path" "$s32x" "$diff_dir/$name.out" "${run_args[@]}")
        normalize_output < "$diff_dir/$name.out" > "$diff_dir/$name.norm"

        if ! cmp -s "$diff_dir/ref.norm" "$diff_dir/$name.norm"; then
            verdict="diverge"
            details="$details [$name: OUTPUT differs, see $diff_dir/{ref,$name}.norm]"
        elif [ "$cmp_rc" = "1" ] && [ "$rc" != "$ref_rc" ]; then
            verdict="diverge"
            details="$details [$name: exit $rc vs ref $ref_rc]"
        fi
    done

    if [ "$verdict" = "agree" ]; then
        echo -e "${GREEN}AGREE${NC}$details"
        AGREE=$((AGREE + 1))
    else
        echo -e "${RED}DIVERGE${NC}$details"
        DIVERGED=$((DIVERGED + 1))
        DIVERGED_TESTS+=("$test_name")
    fi
}

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
echo "Differential results: $AGREE agree, $DIVERGED diverged, $SKIPPED skipped (of $TOTAL)"
if [ ${#SKIPPED_ENGINES[@]} -gt 0 ]; then
    echo -e "${YELLOW}NOTE${NC}: engines not compared this run: ${SKIPPED_ENGINES[*]}"
fi
if [ $DIVERGED -gt 0 ]; then
    echo -e "${RED}Divergent tests:${NC} ${DIVERGED_TESTS[*]}"
    if [ -n "${ALLOW_KNOWN_DIVERGENCES:-}" ]; then
        UNEXPECTED=()
        for t in "${DIVERGED_TESTS[@]}"; do
            if ! echo "$KNOWN_DIVERGENT" | grep -qx "$t"; then
                UNEXPECTED+=("$t")
            fi
        done
        if [ ${#UNEXPECTED[@]} -eq 0 ]; then
            echo -e "${YELLOW}All divergences are known/accepted${NC} (ALLOW_KNOWN_DIVERGENCES)."
            exit 0
        fi
        echo -e "${RED}UNEXPECTED divergences:${NC} ${UNEXPECTED[*]}"
    fi
    exit 1
fi
echo -e "${GREEN}All engines agree.${NC}"
exit 0
