#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if TOP=$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null); then
    ROOT="$TOP"
else
    ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
fi
DBT="${DBT:-$ROOT/tools/dbt/slow32-dbt}"
DIAG_ON_MISMATCH="${DBT_DIAG_ON_MISMATCH:-0}"
DIAG_BLOCK_PC="${DBT_DIAG_BLOCK_PC:-}"
DIAG_BRANCH_PC="${DBT_DIAG_BRANCH_PC:-}"
DIAG_MAX="${DBT_DIAG_MAX:-64}"
KEEP_TMP="${DBT_KEEP_TMP:-0}"
REQUIRE_MATCH="${DBT_REQUIRE_MATCH:-0}"

if [[ ! -x "$DBT" ]]; then
    echo "error: missing executable: $DBT" >&2
    exit 1
fi

if [[ $# -gt 0 ]]; then
    TESTS=("$@")
else
    TESTS=(
        "$ROOT/regression/results/feature-branches/test.s32x"
        "$ROOT/regression/results/bench-loops/test.s32x"
        "$ROOT/regression/results/feature-crc32/test.s32x"
        "$ROOT/regression/results/feature-strtod/test.s32x"
    )
fi

tmpdir=$(mktemp -d /tmp/compare-stage5-safe-unsafe.XXXXXX)
if [[ "$KEEP_TMP" == "0" ]]; then
    trap 'rm -rf "$tmpdir"' EXIT
else
    echo "keeping tmpdir: $tmpdir" >&2
fi

printf "%-20s %-6s %-8s %-8s\n" "test" "rc" "safe(s)" "unsf(s)"
mismatch_count=0

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing $t" >&2
        continue
    fi

    base=$(basename "$(dirname "$t")")
    out_safe="$tmpdir/${base}.safe.out"
    err_safe="$tmpdir/${base}.safe.err"
    out_unsafe="$tmpdir/${base}.unsafe.out"
    err_unsafe="$tmpdir/${base}.unsafe.err"

    set +e
    SLOW32_DBT_STAGE5_SIDE_EXIT=1 \
    SLOW32_DBT_STAGE5_SIDE_EXIT_MODE=eqne_u \
    "$DBT" -5 -G -W -s "$t" >"$out_safe" 2>"$err_safe"
    rc_safe=$?

    SLOW32_DBT_STAGE5_SIDE_EXIT=1 \
    SLOW32_DBT_STAGE5_SIDE_EXIT_MODE=eqne_u \
    SLOW32_DBT_STAGE5_SIDE_EXIT_FAMILY=B \
    SLOW32_DBT_STAGE5_ALLOW_UNSAFE_FAMILY_B=1 \
    "$DBT" -5 -G -W -s "$t" >"$out_unsafe" 2>"$err_unsafe"
    rc_unsafe=$?
    set -e

    rc="ok"
    if [[ $rc_safe -ne $rc_unsafe ]]; then
        rc="rc!"
    fi
    if ! cmp -s "$out_safe" "$out_unsafe"; then
        rc="out!"
    fi

    if [[ "$DIAG_ON_MISMATCH" != "0" && "$rc" != "ok" ]]; then
        diag_out="$tmpdir/${base}.diag.out"
        diag_err="$tmpdir/${base}.diag.err"
        diag_env=(
            "SLOW32_DBT_STAGE5_SIDE_EXIT=1"
            "SLOW32_DBT_STAGE5_SIDE_EXIT_MODE=eqne_u"
            "SLOW32_DBT_STAGE5_SIDE_EXIT_FAMILY=B"
            "SLOW32_DBT_STAGE5_ALLOW_UNSAFE_FAMILY_B=1"
        )
        if [[ -n "$DIAG_BLOCK_PC" ]]; then
            diag_env+=("SLOW32_DBT_TRACE_TRANSLATED_BLOCK_PC=$DIAG_BLOCK_PC")
            diag_env+=("SLOW32_DBT_TRACE_TRANSLATED_BLOCK_MAX=$DIAG_MAX")
            diag_env+=("SLOW32_DBT_TRACE_BLOCK_EXITS_PC=$DIAG_BLOCK_PC")
            diag_env+=("SLOW32_DBT_TRACE_BLOCK_EXITS_MAX=$DIAG_MAX")
            diag_env+=("SLOW32_DBT_TRACE_BLOCK_REGS_PC=$DIAG_BLOCK_PC")
            diag_env+=("SLOW32_DBT_TRACE_BLOCK_REGS_MAX=$DIAG_MAX")
        fi
        if [[ -n "$DIAG_BRANCH_PC" ]]; then
            diag_env+=("SLOW32_DBT_TRACE_DEFERRED_EXIT_PC=$DIAG_BRANCH_PC")
            diag_env+=("SLOW32_DBT_TRACE_DEFERRED_EXIT_MAX=$DIAG_MAX")
            diag_env+=("SLOW32_DBT_TRACE_EXIT_SLOT_PC=$DIAG_BRANCH_PC")
            diag_env+=("SLOW32_DBT_TRACE_EXIT_SLOT_MAX=$DIAG_MAX")
            diag_env+=("SLOW32_DBT_TRACE_BACKEDGE_DIRTY_PC=$DIAG_BRANCH_PC")
            diag_env+=("SLOW32_DBT_TRACE_BACKEDGE_DIRTY_MAX=$DIAG_MAX")
            diag_env+=("SLOW32_DBT_TRACE_SIDE_EXIT_POLICY_PC=$DIAG_BRANCH_PC")
            diag_env+=("SLOW32_DBT_TRACE_SIDE_EXIT_POLICY_MAX=$DIAG_MAX")
        fi

        set +e
        env "${diag_env[@]}" "$DBT" -5 -G -W -s "$t" >"$diag_out" 2>"$diag_err"
        diag_rc=$?
        set -e
        echo "diag[$base]: rc=$diag_rc logs: $diag_out $diag_err" >&2
    fi
    if [[ "$rc" != "ok" ]]; then
        mismatch_count=$((mismatch_count + 1))
    fi

    ts=$(awk '/^Time: / {print $2}' "$err_safe" | tail -n1)
    tu=$(awk '/^Time: / {print $2}' "$err_unsafe" | tail -n1)
    [[ -n "$ts" ]] || ts="-"
    [[ -n "$tu" ]] || tu="-"
    printf "%-20s %-6s %-8s %-8s\n" "$base" "$rc" "$ts" "$tu"
done

echo
echo "summary: mismatches=$mismatch_count"
if [[ "$REQUIRE_MATCH" != "0" && $mismatch_count -ne 0 ]]; then
    exit 2
fi
