#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
STRESS="$ROOT/tools/dbt/scripts/stress-stage5-safe-unsafe.sh"
REQUIRE_STABLE="${SWEEP_REQUIRE_STABLE:-0}"
KEEP_TMP="${DBT_KEEP_TMP:-0}"

if [[ ! -x "$STRESS" ]]; then
    echo "error: missing executable script: $STRESS" >&2
    exit 1
fi

ITERS="${1:-100}"
shift || true

if [[ $# -gt 0 ]]; then
    TESTS=("$@")
else
    TESTS=()
    for t in \
        "$ROOT/regression/results/feature-branches/test.s32x" \
        "$ROOT/regression/results/bench-loops/test.s32x" \
        "$ROOT/regression/results/feature-crc32/test.s32x" \
        "$ROOT/regression/results/feature-strtod/test.s32x"; do
        [[ -f "$t" ]] && TESTS+=("$t")
    done
    if [[ ${#TESTS[@]} -eq 0 ]]; then
        echo "error: no default regression .s32x files found under regression/results" >&2
        exit 1
    fi
fi

declare -a MODES=(
    "default|"
    "guard_all|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=all"
    "guard_jcc|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=jcc"
    "guard_jcc_immimm_on|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=jcc SLOW32_DBT_PEEPHOLE_CALL_ALLOW_IMMIMM=1"
    "guard_none|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=none"
    "guard_none_no_immimm|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=none SLOW32_DBT_NO_PEEPHOLE_IMMIMM=1"
    "guard_none_no_jcc|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=none SLOW32_DBT_NO_PEEPHOLE_JCC=1"
    "guard_none_no_jcc_no_immimm|SLOW32_DBT_PEEPHOLE_GUARD_CALLS=none SLOW32_DBT_NO_PEEPHOLE_JCC=1 SLOW32_DBT_NO_PEEPHOLE_IMMIMM=1"
)

TMPDIR=$(mktemp -d /tmp/sweep-peephole.XXXXXX)
if [[ "$KEEP_TMP" = "0" ]]; then
    trap 'rm -rf "$TMPDIR"' EXIT
fi

printf "%-24s %-6s %-12s\n" "mode" "rc" "notes"

stable_first=""
unstable_modes=()

for row in "${MODES[@]}"; do
    name="${row%%|*}"
    envs="${row#*|}"
    rc=0
    notes="ok"

    set +e
    if [[ -n "$envs" ]]; then
        # shellcheck disable=SC2086
        env DBT_KEEP_TMP=1 $envs bash "$STRESS" "$ITERS" "${TESTS[@]}" >"$TMPDIR/$name.out" 2>"$TMPDIR/$name.err"
    else
        env DBT_KEEP_TMP=1 bash "$STRESS" "$ITERS" "${TESTS[@]}" >"$TMPDIR/$name.out" 2>"$TMPDIR/$name.err"
    fi
    rc=$?
    set -e

    if [[ $rc -ne 0 ]]; then
        notes=$(rg -n "mismatch:|failed to run command|command not found" "$TMPDIR/$name.err" | head -n1 | sed 's/^[0-9]*://')
        if [[ -z "$notes" ]]; then
            notes=$(tail -n 1 "$TMPDIR/$name.err" | tr -d '\n')
        fi
        [[ -n "$notes" ]] || notes="failed"
    fi
    printf "%-24s %-6s %-12s\n" "$name" "$rc" "$notes"
    if [[ $rc -eq 0 ]]; then
        if [[ -z "$stable_first" ]]; then
            stable_first="$name"
        fi
    else
        unstable_modes+=("$name")
    fi
done

echo
if [[ -n "$stable_first" ]]; then
    echo "recommended: $stable_first"
fi
if [[ ${#unstable_modes[@]} -gt 0 ]]; then
    echo "unstable: ${unstable_modes[*]}"
    if [[ "$REQUIRE_STABLE" != "0" ]]; then
        exit 2
    fi
else
    echo "unstable: none"
fi

if [[ "$KEEP_TMP" != "0" ]]; then
    echo "artifacts: $TMPDIR"
fi
