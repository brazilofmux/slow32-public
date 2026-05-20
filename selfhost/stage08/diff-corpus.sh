#!/usr/bin/env bash
# Differential codegen harness for the stage08 self-host bug surface.
#
# The s12cc compiler is built three ways during a fixed-point bootstrap:
#   cc.s32x       — host LLVM build (correct codegen reference)
#   gen1_cc.s32x  — stage07 compiler's compile of s12cc.c
#   fp-gen2.s32x  — gen1_cc's compile of s12cc.c (the fixed-point gate
#                   compares this with fp-gen3.s32x)
#
# All three artifacts share the same source. When they compile a tiny C
# program they should produce identical assembly. When they don't, the
# diff localizes which compiler internal got miscompiled in the suspect
# binary — much faster than re-running the 1–2 min full bootstrap.
#
# Typical workflow:
#
#   # Build the suspect binary once (slow path).
#   selfhost/stage08/run-tests.sh --fixed-point --keep-artifacts
#   WD=$(ls -dt /tmp/selfhost-v2-stage08.* | head -1)
#
#   # Iterate on hypotheses against the corpus (fast path).
#   selfhost/stage08/diff-corpus.sh --test "$WD/fp-gen2.s32x"
#
#   # Or compare two binaries directly:
#   selfhost/stage08/diff-corpus.sh \
#       --ref selfhost/stage08/cc.s32x \
#       --test "$WD/gen1_cc.s32x" --verbose

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

REF_CC="${REF_CC:-$SCRIPT_DIR/cc.s32x}"
TEST_CC="${TEST_CC:-}"
EMU="${SELFHOST_EMU:-}"
CORPUS_DIR="${CORPUS_DIR:-$SCRIPT_DIR/diff-corpus}"
CC_TIMEOUT="${CC_TIMEOUT:-120}"
KEEP=0
VERBOSE=0
TESTS=()

choose_default_emu() {
    # Stage08 policy: only the environment variable or the minimal trusted
    # stage00 emulator.  No automatic preference for the DBT or slow32-fast.
    local stage0="$SELFHOST_DIR/stage00/s32-emu"
    if [[ -x "$stage0" ]]; then
        printf '%s\n' "$stage0"
        return
    fi
    return 1
}

usage() {
    cat <<USAGE
Usage: $0 [options] [test.c ...]

Compile each corpus C file with two SLOW-32 compilers and diff the
resulting assembly. Reports a punch list of which corpus files diverge.

Options:
  --ref <path>       Reference compiler (default: selfhost/stage08/cc.s32x)
  --test <path>      Suspect compiler (REQUIRED — no default)
  --emu <path>       Emulator (stage08 policy: SELFHOST_EMU or stage00/s32-emu only)
  --corpus <dir>     Corpus directory (default: selfhost/stage08/diff-corpus)
  --timeout <secs>   Per-compile timeout (default: 120, env: CC_TIMEOUT)
  --keep             Keep WORKDIR after exit (artifacts and diff files)
  --verbose          Print the unified diff for each differing file
  -h, --help         Show this help

If no test files are passed on the command line, every *.c file in
--corpus is used.

Exit codes:
  0  All corpus entries produced byte-identical assembly.
  1  At least one corpus entry diverged.
  2  Setup error (missing binary, etc.).
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --ref) REF_CC="$2"; shift 2 ;;
        --test) TEST_CC="$2"; shift 2 ;;
        --emu) EMU="$2"; shift 2 ;;
        --corpus) CORPUS_DIR="$2"; shift 2 ;;
        --timeout) CC_TIMEOUT="$2"; shift 2 ;;
        --keep) KEEP=1; shift ;;
        --verbose) VERBOSE=1; shift ;;
        -h|--help) usage; exit 0 ;;
        --) shift; break ;;
        -*) echo "Unknown option: $1" >&2; usage; exit 2 ;;
        *) TESTS+=("$1"); shift ;;
    esac
done

while [[ $# -gt 0 ]]; do
    TESTS+=("$1"); shift
done

if [[ -z "$EMU" ]]; then
    EMU="$(choose_default_emu)" || {
        echo "No emulator found (stage08 policy: set SELFHOST_EMU or ensure stage00/s32-emu exists)" >&2
        exit 2
    }
fi

if [[ -z "$TEST_CC" ]]; then
    echo "ERROR: --test <path> is required (suspect compiler to compare against --ref)" >&2
    echo "Hint: pass a path like /tmp/selfhost-v2-stage08.*/fp-gen2.s32x" >&2
    exit 2
fi

[[ -f "$REF_CC" ]] || { echo "Missing reference compiler: $REF_CC" >&2; exit 2; }
[[ -f "$TEST_CC" ]] || { echo "Missing test compiler: $TEST_CC" >&2; exit 2; }
[[ -f "$EMU" ]] || { echo "Missing emulator: $EMU" >&2; exit 2; }

if [[ ${#TESTS[@]} -eq 0 ]]; then
    [[ -d "$CORPUS_DIR" ]] || { echo "Missing corpus dir: $CORPUS_DIR" >&2; exit 2; }
    while IFS= read -r f; do
        TESTS+=("$f")
    done < <(find "$CORPUS_DIR" -maxdepth 1 -name '*.c' | LC_ALL=C sort)
fi

if [[ ${#TESTS[@]} -eq 0 ]]; then
    echo "No corpus files found" >&2
    exit 2
fi

WORKDIR="$(mktemp -d /tmp/diff-corpus.XXXXXX)"
if [[ "$KEEP" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

printf "Differential codegen harness\n"
printf "  ref:     %s\n" "$REF_CC"
printf "  test:    %s\n" "$TEST_CC"
printf "  emu:     %s\n" "$EMU"
printf "  workdir: %s\n" "$WORKDIR"
printf "\n"

printf "%-28s %-12s %s\n" "Test" "Status" "Notes"
printf "%-28s %-12s %s\n" "----------------------------" "------------" "-----"

run_compile() {
    local cc="$1"
    local src="$2"
    local out_s="$3"
    local log="$4"
    local rc=0
    timeout "$CC_TIMEOUT" "$EMU" "$cc" "$src" "$out_s" >"$log" 2>&1 || rc=$?
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        return 1
    fi
    [[ -s "$out_s" ]] || return 1
    if grep -Eq "Execute fault|Memory fault|Write out of bounds|Unknown opcode|Unknown instruction|Load fault|Store fault|Execution limit reached" "$log"; then
        return 1
    fi
    return 0
}

DIFF_COUNT=0
SAME_COUNT=0
FAIL_COUNT=0
DIFFERED=()

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        printf "%-28s %-12s missing source: %s\n" "$(basename "$t" .c)" "SKIP" "$t"
        continue
    fi
    base="$(basename "$t" .c)"
    ref_asm="$WORKDIR/${base}.ref.s"
    test_asm="$WORKDIR/${base}.test.s"
    diff_out="$WORKDIR/${base}.diff"
    ref_log="$WORKDIR/${base}.ref.log"
    test_log="$WORKDIR/${base}.test.log"

    if ! run_compile "$REF_CC" "$t" "$ref_asm" "$ref_log"; then
        printf "%-28s %-12s %s\n" "$base" "REF_FAIL" "$ref_log"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi
    if ! run_compile "$TEST_CC" "$t" "$test_asm" "$test_log"; then
        printf "%-28s %-12s %s\n" "$base" "TEST_FAIL" "$test_log"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi

    if cmp -s "$ref_asm" "$test_asm"; then
        printf "%-28s %-12s\n" "$base" "SAME"
        SAME_COUNT=$((SAME_COUNT + 1))
    else
        diff -u "$ref_asm" "$test_asm" >"$diff_out" || true
        nlines="$(wc -l <"$diff_out" | tr -d ' ')"
        printf "%-28s %-12s %s lines (%s)\n" "$base" "DIFFER" "$nlines" "$diff_out"
        DIFF_COUNT=$((DIFF_COUNT + 1))
        DIFFERED+=("$base")
        if [[ "$VERBOSE" -eq 1 ]]; then
            sed 's/^/    /' "$diff_out"
            echo
        fi
    fi
done

echo
printf "Summary: %d same, %d differ, %d failed (%d total)\n" \
    "$SAME_COUNT" "$DIFF_COUNT" "$FAIL_COUNT" "${#TESTS[@]}"

if [[ "$DIFF_COUNT" -gt 0 ]]; then
    echo "Differing tests:"
    for d in "${DIFFERED[@]}"; do
        printf "  %s\n" "$d"
    done
fi

if [[ "$KEEP" -eq 1 ]]; then
    printf "\nArtifacts kept: %s\n" "$WORKDIR"
fi

if [[ "$FAIL_COUNT" -gt 0 || "$DIFF_COUNT" -gt 0 ]]; then
    exit 1
fi
exit 0
