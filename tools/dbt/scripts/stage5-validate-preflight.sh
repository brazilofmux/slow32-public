#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
DBT="${DBT:-$ROOT/tools/dbt/slow32-dbt}"

if [[ ! -x "$DBT" ]]; then
    echo "error: missing executable: $DBT" >&2
    exit 1
fi

if [[ $# -gt 0 ]]; then
    TESTS=("$@")
else
    TESTS=()
    for t in \
        "$ROOT/regression/results/feature-strtod/test.s32x" \
        "$ROOT/regression/results/feature-crc32/test.s32x" \
        "$ROOT/regression/results/feature-branches/test.s32x" \
        "$ROOT/regression/results/feature-function-calls/test.s32x"; do
        [[ -f "$t" ]] && TESTS+=("$t")
    done
    if [[ ${#TESTS[@]} -eq 0 ]]; then
        echo "error: no default preflight tests found under regression/results" >&2
        exit 1
    fi
fi

tmpdir=$(mktemp -d /tmp/stage5-validate-preflight.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT

echo "stage5-validate-preflight: tests=${#TESTS[@]}"
printf "%-20s %-6s %-8s %-8s %-9s %-8s\n" "test" "rc" "mismatch" "eligible" "attempted" "cover%"

total_eligible=0
total_attempted=0

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing $t" >&2
        continue
    fi
    base=$(basename "$(dirname "$t")")
    out="$tmpdir/$base.out"
    err="$tmpdir/$base.err"

    set +e
    SLOW32_DBT_STAGE5_VALIDATE_LIFT=1 \
    SLOW32_DBT_STAGE5_VALIDATE_REQUIRE=1 \
    "$DBT" -5 -G -W -s "$t" >"$out" 2>"$err"
    rc=$?
    set -e

    mismatch="0"
    eligible="-"
    attempted="-"
    cover="-"
    if rg -q "stage5-validate mismatch" "$err"; then
        mismatch="1"
    fi
    if rg -q "Stage5 validate eligible:" "$err"; then
        eligible=$(awk '/Stage5 validate eligible:/ {print $4}' "$err" | tail -n1)
    fi
    if rg -q "Stage5 validate attempted:" "$err"; then
        attempted=$(awk '/Stage5 validate attempted:/ {print $4}' "$err" | tail -n1)
    fi
    if [[ "$eligible" != "-" && "$attempted" != "-" ]]; then
        total_eligible=$((total_eligible + eligible))
        total_attempted=$((total_attempted + attempted))
        if [[ "$attempted" -gt 0 ]]; then
            cover=$(awk -v e="$eligible" -v a="$attempted" 'BEGIN { printf "%.1f", (100.0 * e) / a }')
        else
            cover="0.0"
        fi
    fi

    printf "%-20s %-6s %-8s %-8s %-9s %-8s\n" "$base" "$rc" "$mismatch" "$eligible" "$attempted" "$cover"

    if [[ $rc -ne 0 ]]; then
        echo "failure: $base (rc=$rc)" >&2
        echo "  logs: $out $err" >&2
        exit $rc
    fi
    if [[ "$mismatch" != "0" ]]; then
        echo "failure: validator mismatch in $base" >&2
        echo "  logs: $out $err" >&2
        exit 2
    fi
done

if [[ "$total_attempted" -gt 0 ]]; then
    total_cover=$(awk -v e="$total_eligible" -v a="$total_attempted" 'BEGIN { printf "%.1f", (100.0 * e) / a }')
    echo "stage5-validate-preflight: coverage eligible/attempted=${total_eligible}/${total_attempted} (${total_cover}%)"
fi

echo "stage5-validate-preflight: all ok"
