#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "usage: $0 <program.s32x> [runs]" >&2
    exit 1
fi

PROG="$1"
RUNS="${2:-20}"

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if TOP=$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null); then
    ROOT="$TOP"
else
    ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
fi

BENCH="${BENCH:-$ROOT/tools/dbt/scripts/bench-stage4-stage5.sh}"
JAL_JUMP_CAPS="${JAL_JUMP_CAPS:-2 3 4 6}"
JAL_CALL_CAPS="${JAL_CALL_CAPS:-2 4 6}"
JAL_CALL_LONG_CAPS="${JAL_CALL_LONG_CAPS:-0 8 12}"

if [[ ! -x "$BENCH" ]]; then
    echo "error: missing executable script: $BENCH" >&2
    exit 1
fi

printf "%-4s %-4s %-4s %-9s %-9s %-7s %-7s %-7s %-8s %-6s\n" \
  "JJ" "CS" "CL" "t4_mean" "t5_mean" "emit" "bpg" "crtn" "jjpol" "flag"

for jj in $JAL_JUMP_CAPS; do
    for cs in $JAL_CALL_CAPS; do
        for cl in $JAL_CALL_LONG_CAPS; do
            out=$(
                SLOW32_DBT_STAGE5_BENCH_MAX_JAL_JUMP_GINST="$jj" \
                SLOW32_DBT_STAGE5_BENCH_MAX_JAL_CALL_GINST="$cs" \
                SLOW32_DBT_STAGE5_BENCH_MAX_JAL_CALL_LONG_GINST="$cl" \
                "$BENCH" "$PROG" "$RUNS"
            )

            t4=$(awk '/^stage4:/ {for(i=1;i<=NF;i++) if($i ~ /^mean=/){sub(/^mean=/,"",$i); print $i}}' <<<"$out")
            t5=$(awk '/^stage5:/ {for(i=1;i<=NF;i++) if($i ~ /^mean=/){sub(/^mean=/,"",$i); print $i}}' <<<"$out")
            emit=$(awk '/^Stage5 emit success:/ {print $4}' <<<"$out")
            bpg=$(awk '/^Stage5 emit bytes\/guest-inst:/ {print $4}' <<<"$out")
            crtn=$(awk '/^  emit policy call_return:/ {print $4}' <<<"$out")
            jjpol=$(awk '/^  emit policy bench_jal_jump:/ {print $4}' <<<"$out")

            [[ -n "$t4" ]] || t4="-"
            [[ -n "$t5" ]] || t5="-"
            [[ -n "$emit" ]] || emit="-"
            [[ -n "$bpg" ]] || bpg="-"
            [[ -n "$crtn" ]] || crtn="0"
            [[ -n "$jjpol" ]] || jjpol="0"

            flag="ok"
            if [[ "$t5" != "-" ]]; then
                if awk "BEGIN{exit !($t5 >= 0.069)}"; then
                    flag="bad"
                fi
            fi
            printf "%-4s %-4s %-4s %-9s %-9s %-7s %-7s %-7s %-8s %-6s\n" \
              "$jj" "$cs" "$cl" "$t4" "$t5" "$emit" "$bpg" "$crtn" "$jjpol" "$flag"
        done
    done
done
