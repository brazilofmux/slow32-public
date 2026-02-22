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
    mapfile -t TESTS < <(find "$ROOT/regression/results" -name '*.s32x' -type f | sort)
fi

tmp=$(mktemp)
tmp_side=$(mktemp)
tmp_unsup=$(mktemp)
tmpdir=$(mktemp -d /tmp/stage5-aggregate.XXXXXX)
trap 'rm -f "$tmp" "$tmp_side" "$tmp_unsup"; rm -rf "$tmpdir"' EXIT

for t in "${TESTS[@]}"; do
    [[ -f "$t" ]] || continue
    base=$(basename "$(dirname "$t")")
    err="$tmpdir/${base}.err"
    set +e
    "$DBT" -5 -G -W -s "$t" >/dev/null 2>"$err"
    rc=$?
    set -e
    if [[ $rc -ne 0 ]]; then
        fault=$(grep -E '^DBT: (Memory fault|Unknown instruction)' "$err" | tail -n1 || true)
        if [[ -z "$fault" ]]; then
            echo "skip $base rc=$rc" >&2
            continue
        fi
    fi

    awk -v test="$base" '
      /^  emit pattern / {
        # Format:
        #   emit pattern <name> n=... ginst=... hbytes=... bpg=... [avg_g=... avg_h=...]
        name=$3
        g=0
        h=0
        b=0
        for (i=1; i<=NF; i++) {
          if ($i ~ /^ginst=/) { g=$i; sub(/^ginst=/, "", g) }
          if ($i ~ /^hbytes=/){ h=$i; sub(/^hbytes=/, "", h) }
          if ($i ~ /^bpg=/)   { b=$i; sub(/^bpg=/, "", b) }
        }
        if (name != "" && g+0 > 0) {
          printf "%s\t%s\t%s\t%s\t%s\n", name, g, h, b, test
        }
      }
    ' "$err" >> "$tmp"

    awk -v test="$base" '
      /^  emit side_exit_regions:/ {
        total=owned=unsupported=disabled=call_guard=0
        for (i=1; i<=NF; i++) {
          if ($i ~ /^total=/)       { x=$i; sub(/^total=/, "", x); total=x+0 }
          if ($i ~ /^owned=/)       { x=$i; sub(/^owned=/, "", x); owned=x+0 }
          if ($i ~ /^unsupported=/) { x=$i; sub(/^unsupported=/, "", x); unsupported=x+0 }
          if ($i ~ /^disabled=/)    { x=$i; sub(/^disabled=/, "", x); disabled=x+0 }
          if ($i ~ /^call_guard=/)  { x=$i; sub(/^call_guard=/, "", x); call_guard=x+0 }
        }
        printf "%s\t%d\t%d\t%d\t%d\t%d\n",
               test, total, owned, unsupported, disabled, call_guard
      }
    ' "$err" >> "$tmp_side"

    awk -v test="$base" '
      /^  emit side_exit unsupported top[0-9]+:/ {
        op=""; cnt=0
        for (i=1; i<=NF; i++) {
          if ($i ~ /^op=/)    { x=$i; sub(/^op=/, "", x); op=x }
          if ($i ~ /^count=/) { x=$i; sub(/^count=/, "", x); cnt=x+0 }
        }
        if (op != "" && cnt > 0) {
          printf "%s\t%s\t%d\n", test, op, cnt
        }
      }
    ' "$err" >> "$tmp_unsup"
done

if [[ ! -s "$tmp" ]]; then
    echo "No Stage5 pattern rows collected."
    exit 0
fi

echo "Pattern aggregates (all selected tests):"
awk -F'\t' '
  {
    p=$1; g=$2+0; h=$3+0; c[p]++; G[p]+=g; H[p]+=h;
    if ($4+0 > maxb[p]) { maxb[p]=$4+0; maxtest[p]=$5 }
  }
  END {
    for (p in c) {
      bpg=(G[p]>0)?(H[p]/G[p]):0;
      avg_g=G[p]/c[p];
      avg_h=H[p]/c[p];
      printf "%s\t%.6f\tn=%d\tginst=%d\thbytes=%d\tbpg=%.2f\tavg_g=%.2f\tavg_h=%.2f\tmax_bpg=%.2f(%s)\n",
             p, bpg, c[p], G[p], H[p], bpg, avg_g, avg_h, maxb[p], maxtest[p];
    }
  }
' "$tmp" | sort -t$'\t' -k2,2nr | cut -f1,3-

if [[ -s "$tmp_side" ]]; then
    echo
    echo "Side-exit region totals (all selected tests):"
    awk -F'\t' '
      { T+=$2; O+=$3; U+=$4; D+=$5; C+=$6; N++ }
      END {
        printf "tests=%d total=%d owned=%d unsupported=%d disabled=%d call_guard=%d\n",
               N, T, O, U, D, C
      }
    ' "$tmp_side"
fi

if [[ -s "$tmp_unsup" ]]; then
    echo
    echo "Unsupported side-exit opcodes (aggregated top):"
    awk -F'\t' '
      { op=$2; c[op]+=$3 }
      END {
        for (op in c) {
          printf "%s\t%d\n", op, c[op]
        }
      }
    ' "$tmp_unsup" | sort -t$'\t' -k2,2nr | head -n 8
fi
