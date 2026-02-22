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

tmpdir=$(mktemp -d /tmp/compare-stage4-stage5.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT

printf "%-20s %-5s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-8s\n" \
  "test" "rc" "t4(s)" "t5(s)" "sel" "emit" "ginst" "b/gi" "jret_s" "jret_l" "jind"

for t in "${TESTS[@]}"; do
    if [[ ! -f "$t" ]]; then
        echo "skip: missing $t" >&2
        continue
    fi

    base=$(basename "$(dirname "$t")")
    out4="$tmpdir/${base}.s4.out"
    err4="$tmpdir/${base}.s4.err"
    out5="$tmpdir/${base}.s5.out"
    err5="$tmpdir/${base}.s5.err"

    set +e
    "$DBT" -4 -s "$t" >"$out4" 2>"$err4"
    rc4=$?
    "$DBT" -5 -G -W -s "$t" >"$out5" 2>"$err5"
    rc5=$?
    set -e

    rc="ok"
    if [[ $rc4 -ne $rc5 ]]; then
        fault4=$(grep -E '^DBT: (Memory fault|Unknown instruction)' "$err4" | tail -n1 || true)
        fault5=$(grep -E '^DBT: (Memory fault|Unknown instruction)' "$err5" | tail -n1 || true)
        if [[ -n "$fault4" && "$fault4" == "$fault5" ]]; then
            rc="fault"
        else
            rc="rc!"
        fi
    fi
    if ! cmp -s "$out4" "$out5"; then
        rc="out!"
    fi

    t4=$(awk '/^Time: / {print $2}' "$err4" | tail -n1)
    t5=$(awk '/^Time: / {print $2}' "$err5" | tail -n1)
    sel=$(awk '/^Stage5 BURG selected:/ {print $4}' "$err5" | tail -n1)
    emit=$(awk '/^Stage5 emit success:/ {print $4}' "$err5" | tail -n1)
    ginst=$(awk '/^Stage5 emit guest insts:/ {print $5}' "$err5" | tail -n1)
    bgi=$(awk '/^Stage5 emit bytes\/guest-inst:/ {print $4}' "$err5" | tail -n1)
    jret_s=$(awk '/^  emit pattern jalr_ret_short/ {for(i=1;i<=NF;i++) if($i ~ /^bpg=/){sub(/^bpg=/,"",$i); print $i}}' "$err5" | tail -n1)
    jret_l=$(awk '/^  emit pattern jalr_ret_long/ {for(i=1;i<=NF;i++) if($i ~ /^bpg=/){sub(/^bpg=/,"",$i); print $i}}' "$err5" | tail -n1)
    jind=$(awk '/^  emit pattern jalr_indirect/ {for(i=1;i<=NF;i++) if($i ~ /^bpg=/){sub(/^bpg=/,"",$i); print $i}}' "$err5" | tail -n1)

    [[ -n "$t4" ]] || t4="-"
    [[ -n "$t5" ]] || t5="-"
    [[ -n "$sel" ]] || sel="-"
    [[ -n "$emit" ]] || emit="-"
    [[ -n "$ginst" ]] || ginst="-"
    [[ -n "$bgi" ]] || bgi="-"
    [[ -n "$jret_s" ]] || jret_s="-"
    [[ -n "$jret_l" ]] || jret_l="-"
    [[ -n "$jind" ]] || jind="-"

    printf "%-20s %-5s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-8s\n" \
      "$base" "$rc" "$t4" "$t5" "$sel" "$emit" "$ginst" "$bgi" "$jret_s" "$jret_l" "$jind"
done
