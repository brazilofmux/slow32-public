#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "usage: $0 <program.s32x> [runs]" >&2
    exit 1
fi

PROG="$1"
RUNS="${2:-30}"

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if TOP=$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null); then
    ROOT="$TOP"
else
    ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
fi
DBT="${DBT:-$ROOT/tools/dbt/slow32-dbt}"
STAGE4_ARGS="${STAGE4_ARGS:--4}"
STAGE5_ARGS="${STAGE5_ARGS:--N}"

read -r -a S4_ARR <<< "$STAGE4_ARGS"
read -r -a S5_ARR <<< "$STAGE5_ARGS"

if [[ ! -x "$DBT" ]]; then
    echo "error: missing executable: $DBT" >&2
    exit 1
fi
if [[ ! -f "$PROG" ]]; then
    echo "error: missing program: $PROG" >&2
    exit 1
fi

T4=$(mktemp)
T5=$(mktemp)
E4=$(mktemp)
E5=$(mktemp)
O4=$(mktemp)
O5=$(mktemp)
trap 'rm -f "$T4" "$T5" "$E4" "$E5" "$O4" "$O5"' EXIT

for _ in $(seq 1 "$RUNS"); do
    /usr/bin/time -f '%e' "$DBT" "${S4_ARR[@]}" "$PROG" >/dev/null 2>>"$T4"
done
for _ in $(seq 1 "$RUNS"); do
    /usr/bin/time -f '%e' "$DBT" "${S5_ARR[@]}" "$PROG" >/dev/null 2>>"$T5"
done

"$DBT" "${S4_ARR[@]}" -s "$PROG" >"$O4" 2>"$E4"
"$DBT" "${S5_ARR[@]}" -s "$PROG" >"$O5" 2>"$E5"

stats() {
    awk '
      BEGIN { n=0; s=0; ss=0; min=1e9; max=0 }
      /^[[:space:]]*[0-9]+(\.[0-9]+)?[[:space:]]*$/ {
        x=$1+0; n++; s+=x; ss+=x*x; if (x<min) min=x; if (x>max) max=x
      }
      END {
        if (n == 0) {
          printf("n=0 mean=- sd=- min=- max=-");
          exit 0;
        }
        m=s/n; v=(ss/n)-(m*m); if (v<0) v=0; sd=sqrt(v);
        printf("n=%d mean=%.6f sd=%.6f min=%.6f max=%.6f", n, m, sd, min, max);
      }
    ' "$1"
}

echo "program: $PROG"
echo "stage4 args: $STAGE4_ARGS"
echo "stage5 args: $STAGE5_ARGS"
echo "stage4: $(stats "$T4")"
echo "stage5: $(stats "$T5")"

if cmp -s "$O4" "$O5"; then
    echo "output parity: match"
else
    echo "output parity: mismatch"
fi

echo
echo "stage5 selection/emission:"
awk '
  /^Stage5 BURG selected:/ ||
  /^Stage5 BURG sel guest insts:/ ||
  /^Stage5 emit attempted:/ ||
  /^Stage5 emit success:/ ||
  /^Stage5 emit fallback:/ ||
  /^Stage5 emit guest insts:/ ||
  /^Stage5 emit host bytes:/ ||
  /^Stage5 emit bytes\/guest-inst:/ ||
  /^  emit /
' "$E5"
