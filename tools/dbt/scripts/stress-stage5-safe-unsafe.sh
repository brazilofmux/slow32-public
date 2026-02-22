#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if TOP=$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null); then
    ROOT="$TOP"
else
    ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
fi
DBT="${DBT:-$ROOT/tools/dbt/slow32-dbt}"
ITERS="${1:-100}"
shift || true
DBT_SAFE_FLAGS="${DBT_SAFE_FLAGS:--5 -G -W -s}"
DBT_UNSAFE_FLAGS="${DBT_UNSAFE_FLAGS:--5 -G -W -s}"

if [[ ! -x "$DBT" ]]; then
    echo "error: missing executable: $DBT" >&2
    exit 1
fi
if ! [[ "$ITERS" =~ ^[0-9]+$ ]]; then
    echo "error: iteration count must be integer, got '$ITERS'" >&2
    exit 1
fi

if [[ $# -gt 0 ]]; then
    TESTS=("$@")
else
    TESTS=("$ROOT/regression/results/feature-strtod/test.s32x")
fi

KEEP_TMP="${DBT_KEEP_TMP:-0}"
USE_STDBUF="${DBT_USE_STDBUF:-1}"
RUNNER=("$DBT")
if [[ "$USE_STDBUF" != "0" ]] && command -v stdbuf >/dev/null 2>&1; then
    RUNNER=(stdbuf -o0 -e0 "$DBT")
fi
tmpdir=$(mktemp -d /tmp/stress-stage5-safe-unsafe.XXXXXX)
if [[ "$KEEP_TMP" == "0" ]]; then
    trap 'rm -rf "$tmpdir"' EXIT
else
    echo "keeping tmpdir: $tmpdir" >&2
fi

echo "stress: iters=$ITERS tests=${#TESTS[@]} tmpdir=$tmpdir"

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing $t" >&2
        continue
    fi
    base=$(basename "$(dirname "$t")")
    echo "test: $base"

    for ((i = 1; i <= ITERS; i++)); do
        out_safe="$tmpdir/${base}.iter${i}.safe.out"
        err_safe="$tmpdir/${base}.iter${i}.safe.err"
        out_unsafe="$tmpdir/${base}.iter${i}.unsafe.out"
        err_unsafe="$tmpdir/${base}.iter${i}.unsafe.err"

        set +e
        # shellcheck disable=SC2086
        SLOW32_DBT_STAGE5_SIDE_EXIT=1 \
        SLOW32_DBT_STAGE5_SIDE_EXIT_MODE=eqne_u \
        "${RUNNER[@]}" $DBT_SAFE_FLAGS "$t" >"$out_safe" 2>"$err_safe"
        rc_safe=$?

        # shellcheck disable=SC2086
        SLOW32_DBT_STAGE5_SIDE_EXIT=1 \
        SLOW32_DBT_STAGE5_SIDE_EXIT_MODE=eqne_u \
        SLOW32_DBT_STAGE5_SIDE_EXIT_FAMILY=B \
        SLOW32_DBT_STAGE5_ALLOW_UNSAFE_FAMILY_B=1 \
        "${RUNNER[@]}" $DBT_UNSAFE_FLAGS "$t" >"$out_unsafe" 2>"$err_unsafe"
        rc_unsafe=$?
        set -e

        if [[ $rc_safe -ne $rc_unsafe ]] || ! cmp -s "$out_safe" "$out_unsafe"; then
            echo "mismatch: test=$base iter=$i rc_safe=$rc_safe rc_unsafe=$rc_unsafe" >&2
            echo "  safe:   $out_safe $err_safe" >&2
            echo "  unsafe: $out_unsafe $err_unsafe" >&2
            exit 2
        fi

        if (( i % 25 == 0 || i == ITERS )); then
            echo "  iter $i/$ITERS ok"
        fi
    done
done

echo "stress: all ok"
