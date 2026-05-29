#!/usr/bin/env bash
# Verify that stages 1..8 rebuild to bit-identical .s32x artifacts under a
# named emulator, by comparing SHA256 against selfhost/sha256sums.md.
#
# Usage: verify-emu-sums.sh <emu-label> <emu-path> [stage_lo] [stage_hi]
#   emu-label: short tag for logs (e.g. slow32, slow32-fast)
#   emu-path:  absolute path to the emulator binary
#   stage_lo:  first stage number to test (default 1)
#   stage_hi:  last stage number to test (default 8)
#
# Logs land in /tmp/verify-emu-sums-<label>/.  Per-stage logs are <label>/stageNN.log;
# a SUMMARY.txt records OK/FAIL lines for each artifact.
#
# Caveats:
#   - .s32x artifacts are gitignored, so a cascaded build failure leaves the
#     tree partial.  Restore by rebuilding with a known-good emulator before
#     running anything else against the stage tree.
#   - stage01 uses the Forth kernel-on-emulator pipeline; it overrides the
#     Makefile's hard-coded EMU variable to use the requested emulator.
#
# Exit codes: 0 ok, 1 sum mismatch, 2 build failure, 3 setup error.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi
SUMS_FILE="$ROOT/selfhost/sha256sums.md"

LABEL="${1:?emu label required}"
EMU="${2:?emu path required}"
STAGE_LO="${3:-1}"
STAGE_HI="${4:-8}"

[[ -x "$EMU" ]] || { echo "ERR: emu not executable: $EMU" >&2; exit 3; }
[[ -f "$SUMS_FILE" ]] || { echo "ERR: missing $SUMS_FILE" >&2; exit 3; }

LOG_BASE="/tmp/verify-emu-sums-$LABEL"
mkdir -p "$LOG_BASE"
SUMMARY="$LOG_BASE/SUMMARY.txt"
: >"$SUMMARY"

note() { printf '%s\n' "$*" | tee -a "$SUMMARY"; }
fail() { printf 'FAIL: %s\n' "$*" | tee -a "$SUMMARY" >&2; }

note "=== verify-emu-sums label=$LABEL emu=$EMU stages=$STAGE_LO..$STAGE_HI ==="
note "host: $(uname -srm)"
note "started: $(date -Iseconds)"
note ""

# Parse expected sums into associative array EXPECTED["stageNN/file"] = hash
declare -A EXPECTED
current_stage=""
while IFS= read -r line; do
    if [[ "$line" =~ ^"## Stage "([0-9]+) ]]; then
        n="${BASH_REMATCH[1]}"
        current_stage="stage$(printf '%02d' "$n")"
    elif [[ -n "$current_stage" && "$line" =~ ^-\ ([0-9a-f]{64})\ +(.+\.s32x)$ ]]; then
        h="${BASH_REMATCH[1]}"
        f="${BASH_REMATCH[2]}"
        EXPECTED["$current_stage/$f"]="$h"
    fi
done <"$SUMS_FILE"

note "parsed ${#EXPECTED[@]} expected sums"

OVERALL_RC=0

verify_stage() {
    local stage="$1"
    local stage_dir="$ROOT/selfhost/$stage"
    local log="$LOG_BASE/$stage.log"
    local t0 t1
    t0=$(date +%s)

    note ""
    note "--- $stage ---"
    note "log: $log"
    : >"$log"

    cd "$stage_dir" || { fail "$stage: cd failed"; OVERALL_RC=2; return; }

    if ! make clean >>"$log" 2>&1; then
        fail "$stage: make clean failed"
        OVERALL_RC=2
        return
    fi

    local make_env
    if [[ "$stage" == "stage01" ]]; then
        # stage01 references $(EMU) directly; override via make var.
        make_env=(env "EMU=$EMU")
    else
        make_env=(env "SELFHOST_EMU=$EMU")
    fi

    if ! "${make_env[@]}" make >>"$log" 2>&1; then
        fail "$stage: make failed under $LABEL"
        tail -n 40 "$log" | sed 's/^/   | /' | tee -a "$SUMMARY" >&2
        OVERALL_RC=2
        return
    fi

    # Compare sums for every expected file in this stage
    local stage_fail=0
    local key file expected actual
    for key in "${!EXPECTED[@]}"; do
        [[ "$key" == "$stage/"* ]] || continue
        file="${key#$stage/}"
        expected="${EXPECTED[$key]}"
        if [[ ! -f "$file" ]]; then
            fail "$stage/$file: missing after build"
            stage_fail=1
            continue
        fi
        actual=$(sha256sum "$file" | awk '{print $1}')
        if [[ "$actual" == "$expected" ]]; then
            note "  OK    $stage/$file  $actual"
        else
            fail "$stage/$file: expected $expected got $actual"
            stage_fail=1
        fi
    done

    t1=$(date +%s)
    note "  elapsed: $((t1 - t0))s"
    if (( stage_fail )); then
        OVERALL_RC=1
    fi
}

for n in $(seq "$STAGE_LO" "$STAGE_HI"); do
    verify_stage "stage$(printf '%02d' "$n")"
done

note ""
note "finished: $(date -Iseconds)"
case "$OVERALL_RC" in
    0) note "RESULT: PASS — all sums match expected for $LABEL" ;;
    1) note "RESULT: SUM MISMATCH for $LABEL" ;;
    2) note "RESULT: BUILD FAILURE for $LABEL" ;;
    *) note "RESULT: rc=$OVERALL_RC" ;;
esac

exit "$OVERALL_RC"
