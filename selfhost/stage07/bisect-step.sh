#!/usr/bin/env bash
# Bisect helper for Issue #31 — designed for `git bisect run`.
#
# At each commit, this script:
#   1. (Optional) patches hcg_identify_fusions to early-return,
#      isolating any non-fusion bugs in the regalloc/codegen surface.
#   2. Builds gen1_cc and fp-gen2 via run-tests.sh --fixed-point.
#   3. Asks fp-gen2 to compile `int main(void){return 0;}` and checks
#      whether `addi r1, r0, 0` appears in the output.
#
# The third step is the smoking-gun signal from ISSUES.md #31:
# fp-gen2 dropping that single instruction means some function inside
# s12cc.c was miscompiled by gen1_cc.
#
# Exit codes (matching git-bisect-run conventions):
#   0    GOOD — fp-gen2 emits `addi r1, r0, 0`
#   1    BAD  — fp-gen2 missing the addi (or build failed)
#   125  SKIP — couldn't run the test (timeout, missing prereq)
#
# Usage:
#
#   # Sanity-check the script at HEAD before starting the bisect:
#   selfhost/stage07/bisect-step.sh --disable-fusion ; echo "exit=$?"
#   # ISSUES.md predicts BAD (1) at HEAD even with fusion disabled.
#
#   # Sanity-check at the known-good ancestor:
#   git checkout 7b9e1405^
#   selfhost/stage07/bisect-step.sh --disable-fusion ; echo "exit=$?"
#   # Should be GOOD (0) — pre-fusion code, no bug to disable.
#
#   # Drive the actual bisect:  copy this script outside the worktree
#   # first, since older commits don't have it on disk.
#   cp selfhost/stage07/bisect-step.sh /tmp/bisect-step.sh
#   git checkout main
#   git bisect start
#   git bisect bad HEAD
#   git bisect good 7b9e1405^
#   BISECT_ROOT=$PWD git bisect run /tmp/bisect-step.sh --disable-fusion
#
# Without `--disable-fusion` the script tests the raw symptom (does
# fp-gen2 work?) which is BAD across the whole 7b9e1405..HEAD range,
# so won't bisect anywhere useful — keep the flag on.
#
# Caveat: the BAD predicate fires both for "fp-gen2 not built" (gen1_cc
# crash, hit phi/parser limit, etc.) and "fp-gen2 emits wrong code".
# Bisect can land on a commit that introduced the *crash* mode rather
# than the *wrong-code* mode you started with.  Verify the failure
# mode at the identified commit matches the original symptom.
#
# Env knobs:
#   BISECT_ROOT     — slow-32 working tree root (defaults to script's grandparent)
#   BISECT_TIMEOUT  — seconds to wait for run-tests.sh (default 600)

set -uo pipefail   # not -e: we manage every exit explicitly

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${BISECT_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
cd "$ROOT_DIR"

DISABLE_FUSION=0
for arg in "$@"; do
    case "$arg" in
        --disable-fusion) DISABLE_FUSION=1 ;;
        -h|--help)
            sed -n '/^# Bisect helper/,/^# Env knobs:/p' "$0" | sed 's/^# \?//'
            exit 0
            ;;
        *) echo "Unknown arg: $arg" >&2; exit 125 ;;
    esac
done

TIMEOUT="${BISECT_TIMEOUT:-600}"
EMU="$ROOT_DIR/tools/dbt/slow32-dbt"
RUN_TESTS="$ROOT_DIR/selfhost/stage07/run-tests.sh"

if [[ ! -x "$EMU" || ! -x "$RUN_TESTS" ]]; then
    echo "BISECT-SKIP: missing emu or run-tests at this commit" >&2
    exit 125
fi

# Test source: prefer the in-tree corpus file when it exists (committed
# at 68dd34a6); fall back to a /tmp clone for older commits.
TEST_SRC="$ROOT_DIR/selfhost/stage07/diff-corpus/01_return_const.c"
if [[ ! -f "$TEST_SRC" ]]; then
    TEST_SRC="$(mktemp /tmp/bisect-step.XXXXXX.c)"
    cat > "$TEST_SRC" <<'EOF'
int main(void) { return 0; }
EOF
    TEST_SRC_TEMP=1
else
    TEST_SRC_TEMP=0
fi

# Optionally apply the fusion-disable patch. We modify in place and
# unconditionally revert before exit so the worktree stays clean for
# the next bisect step (git bisect needs a clean checkout to advance).
BURG_FILE="$ROOT_DIR/selfhost/stage07/hir_burg.h"
PATCH_BACKUP=""
cleanup() {
    if [[ -n "$PATCH_BACKUP" && -f "$PATCH_BACKUP" ]]; then
        mv -f "$PATCH_BACKUP" "$BURG_FILE"
    fi
    if [[ "$TEST_SRC_TEMP" -eq 1 && -f "$TEST_SRC" ]]; then
        rm -f "$TEST_SRC"
    fi
}
trap cleanup EXIT INT TERM

if [[ "$DISABLE_FUSION" -eq 1 ]]; then
    if [[ ! -f "$BURG_FILE" ]] || \
       ! grep -q "^static void hcg_identify_fusions(void) {" "$BURG_FILE"; then
        # Fusion didn't exist yet at this commit — nothing to disable.
        # Run the test as-is so the bisect can still classify it.
        echo "BISECT-INFO: no fusion at this commit, running unpatched" >&2
        DISABLE_FUSION=0
    fi
fi
if [[ "$DISABLE_FUSION" -eq 1 ]]; then
    PATCH_BACKUP="$(mktemp /tmp/bisect-step.bak.XXXXXX)"
    cp "$BURG_FILE" "$PATCH_BACKUP"
    # Insert `return;` immediately after the opening brace of the
    # function. The clear-loop at the top of the original body is
    # SKIPPED — but ra_extend_fused_cmp ignores hcg_brc_fuse entries
    # left over from a prior function only if they were initialized
    # somewhere. Risk: a stale value from the previous compile. We
    # pre-fill the array via a small awk rewrite that keeps the
    # clear-loop and ONLY skips the fusion-marking pass.
    awk '
        BEGIN { patched = 0 }
        /^static void hcg_identify_fusions\(void\) \{/ { in_fn = 1 }
        in_fn && !patched && /^    \/\* bg_uses is only valid when BURG ran/ {
            print "    return; /* BISECT: fusion disabled (clear loop above still runs) */"
            patched = 1
        }
        { print }
    ' "$PATCH_BACKUP" > "$BURG_FILE"
    if ! grep -q "BISECT: fusion disabled" "$BURG_FILE"; then
        echo "BISECT-SKIP: failed to apply fusion-disable patch" >&2
        exit 125
    fi
fi

# Run the fixed-point gate. Capture pre-existing workdirs so we can
# distinguish a fresh one from a stale leftover.
WD_BEFORE="$(ls -dt /tmp/selfhost-v2-stage07.* 2>/dev/null | head -1 || true)"
LOG="$(mktemp /tmp/bisect-step.log.XXXXXX)"
timeout "$TIMEOUT" "$RUN_TESTS" \
    --emu "$EMU" --fixed-point --keep-artifacts \
    > "$LOG" 2>&1
RC=$?

if [[ "$RC" -eq 124 ]]; then
    echo "BISECT-SKIP: run-tests.sh timed out after ${TIMEOUT}s" >&2
    tail -20 "$LOG" >&2
    rm -f "$LOG"
    exit 125
fi

WD="$(ls -dt /tmp/selfhost-v2-stage07.* 2>/dev/null | head -1 || true)"
if [[ -z "$WD" || "$WD" == "$WD_BEFORE" ]]; then
    echo "BISECT-SKIP: run-tests.sh did not produce a new workdir" >&2
    tail -20 "$LOG" >&2
    rm -f "$LOG"
    exit 125
fi

if [[ ! -s "$WD/fp-gen2.s32x" ]]; then
    echo "BISECT-BAD: fp-gen2.s32x not built (run-tests rc=$RC)" >&2
    tail -20 "$LOG" >&2
    rm -f "$LOG"
    exit 1
fi

# fp-gen2 was built — try to compile the test source with it.
OUT="$(mktemp /tmp/bisect-step.out.XXXXXX.s)"
if ! timeout 120 "$EMU" "$WD/fp-gen2.s32x" "$TEST_SRC" "$OUT" >/dev/null 2>&1; then
    echo "BISECT-BAD: fp-gen2 failed to compile test source" >&2
    rm -f "$OUT" "$LOG"
    exit 1
fi
if [[ ! -s "$OUT" ]]; then
    echo "BISECT-BAD: fp-gen2 produced empty output" >&2
    rm -f "$OUT" "$LOG"
    exit 1
fi

# The smoking-gun check: gen2's output for `int main(void){return 0;}`
# must materialize the constant zero into r1 before jumping to the
# epilogue. The exact emission is `addi r1, r0, 0`.
if grep -Eq '^[[:space:]]*addi r1, r0, 0[[:space:]]*$' "$OUT"; then
    echo "BISECT-GOOD: fp-gen2 emits 'addi r1, r0, 0' for return-zero" >&2
    rm -f "$OUT" "$LOG"
    exit 0
fi

echo "BISECT-BAD: fp-gen2 missing 'addi r1, r0, 0' for return-zero" >&2
echo "--- fp-gen2 output (head) ---" >&2
head -25 "$OUT" >&2
rm -f "$OUT" "$LOG"
exit 1
