#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if TOP=$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null); then
    ROOT="$TOP"
else
    ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
fi
DBT="${DBT:-$ROOT/tools/dbt/slow32-dbt}"

if [[ ! -x "$DBT" ]]; then
    echo "error: missing executable: $DBT" >&2
    exit 1
fi

iters=10
if [[ $# -ge 2 && "$1" == "-n" ]]; then
    iters="$2"
    shift 2
fi

if [[ $# -gt 0 ]]; then
    TESTS=("$@")
else
    TESTS=(
        "$ROOT/regression/results/feature-strtod/test.s32x"
        "$ROOT/regression/results/feature-control-flow/test.s32x"
        "$ROOT/regression/results/bench-loops/test.s32x"
    )
fi

tmpdir=$(mktemp -d /tmp/stage5-determinism.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT

status=0
printf "%-20s %-10s %-10s %-10s\n" "test" "stage4-rc" "stage5-runs" "result"

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing $t" >&2
        continue
    fi

    base=$(basename "$(dirname "$t")")
    out4="$tmpdir/${base}.s4.out"
    err4="$tmpdir/${base}.s4.err"
    "$DBT" -4 -s "$t" >"$out4" 2>"$err4" || true
    rc4=$?

    ok_runs=0
    for i in $(seq 1 "$iters"); do
        out5="$tmpdir/${base}.s5.${i}.out"
        err5="$tmpdir/${base}.s5.${i}.err"
        "$DBT" -5 -G -W -s "$t" >"$out5" 2>"$err5" || true
        rc5=$?

        if [[ $rc5 -ne $rc4 ]]; then
            status=1
            continue
        fi
        if ! cmp -s "$out4" "$out5"; then
            status=1
            continue
        fi
        ok_runs=$((ok_runs + 1))
    done

    result="ok"
    if [[ $ok_runs -ne $iters ]]; then
        result="drift"
        status=1
    fi
    printf "%-20s %-10s %-10s %-10s\n" "$base" "$rc4" "${ok_runs}/${iters}" "$result"
done

if [[ $status -ne 0 ]]; then
    echo
    echo "FAIL: Stage5 determinism drift detected"
    exit 1
fi

echo
echo "OK: Stage5 outputs stable across ${iters} runs per test"
