#!/usr/bin/env bash
set -euo pipefail

# benchmark.sh -- Measure stage05 compiler throughput and verify O(n) scaling
#
# Generates synthetic C files at various sizes, compiles each with stage05 compiler
# on slow32-fast (instruction count + wall time) and optionally slow32-dbt
# (wall time only), then prints a summary table.
#
# Usage:
#   bash benchmark.sh                  # full benchmark with defaults
#   bash benchmark.sh --self-only      # just self-compilation timing
#   bash benchmark.sh --sizes "50 100" # custom size list (KB)
#   bash benchmark.sh --emu PATH       # override slow32-fast path
#   bash benchmark.sh --dbt PATH       # override slow32-dbt path
#   bash benchmark.sh --keep           # preserve generated files

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

# --- Defaults ---
EMU=""
DBT=""
SIZES="50 100 250 500 750 1000"
SELF_ONLY=0
KEEP=0

# --- Parse flags ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --emu)    EMU="$2"; shift 2 ;;
        --dbt)    DBT="$2"; shift 2 ;;
        --sizes)  SIZES="$2"; shift 2 ;;
        --self-only) SELF_ONLY=1; shift ;;
        --keep)   KEEP=1; shift ;;
        *) echo "Unknown flag: $1" >&2; exit 1 ;;
    esac
done

# --- Find emulators ---
if [[ -z "$EMU" ]]; then
    EMU="$ROOT_DIR/tools/emulator/slow32-fast"
    [[ -x "$EMU" ]] || { echo "slow32-fast not found: $EMU" >&2; exit 1; }
fi

if [[ -z "$DBT" ]]; then
    DBT="$ROOT_DIR/tools/dbt/slow32-dbt"
    [[ -x "$DBT" ]] || DBT=""  # optional
fi

# --- Compiler and toolchain ---
COMPILER="$SCRIPT_DIR/cc.s32x"
ASSEMBLER="$SCRIPT_DIR/s32-as.s32x"
[[ -f "$COMPILER" ]] || { echo "compiler not found: $COMPILER" >&2; exit 1; }

WORKDIR="$(mktemp -d /tmp/cc-bench.XXXXXX)"
if [[ "$KEEP" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
else
    echo "Work directory: $WORKDIR"
fi

# --- Synthetic C generator ---
# Generates a .c file of approximately $1 KB.
# Each function is ~300 bytes / ~17 lines.
generate_synthetic() {
    local target_kb="$1"
    local outfile="$2"
    local target_bytes=$((target_kb * 1024))
    local nfuncs=$((target_bytes / 300))
    if [[ "$nfuncs" -lt 1 ]]; then nfuncs=1; fi

    {
        local i=0
        while [[ "$i" -lt "$nfuncs" ]]; do
            cat <<FUNC
int func_${i}(int a, int b) {
    int x;
    int y;
    int i;
    x = a + b;
    y = (a - b) | 7;
    i = 0;
    while (i < 10) {
        if (x > y) {
            x = x - (y >> 2);
        } else {
            y = y + (x & 15);
        }
        i = i + 1;
    }
    return x + y;
}
FUNC
            i=$((i + 1))
        done

        # Add main so it's a valid program
        echo "int main(void) { return func_0(1, 2); }"
    } > "$outfile"
}

# --- Measurement helpers ---
# Run compiler under slow32-fast, extract instructions + wall time
# Returns: instructions wall_seconds output_bytes
run_fast() {
    local input="$1"
    local output="$WORKDIR/bench_out.s"
    local log="$WORKDIR/bench_fast.log"

    rm -f "$output"
    "$EMU" "$COMPILER" "$input" "$output" > "$log" 2>&1 || true

    local insn wall outsz
    insn=$(grep -oP 'Instructions executed: \K[0-9]+' "$log" 2>/dev/null || echo 0)
    wall=$(grep -oP 'Wall time: \K[0-9.]+' "$log" 2>/dev/null || echo 0)
    outsz=0
    if [[ -f "$output" ]]; then
        outsz=$(wc -c < "$output")
    fi
    echo "$insn $wall $outsz"
}

# Run compiler under slow32-dbt -s, extract wall time only
run_dbt() {
    local input="$1"
    local output="$WORKDIR/bench_out_dbt.s"
    local log="$WORKDIR/bench_dbt.log"

    rm -f "$output"
    "$DBT" -s "$COMPILER" "$input" "$output" > "$log" 2>&1 || true

    local wall
    wall=$(grep -oP 'Time: \K[0-9.]+' "$log" 2>/dev/null || echo 0)
    echo "$wall"
}

# --- Collect data ---
declare -a LABELS=()
declare -a LINES_ARR=()
declare -a BYTES_ARR=()
declare -a OUT_ARR=()
declare -a INSN_ARR=()
declare -a INSNB_ARR=()
declare -a KBS_ARR=()
declare -a TFAST_ARR=()
declare -a TDBT_ARR=()

run_one() {
    local label="$1"
    local input="$2"
    local eff_bytes="${3:-}"  # optional: effective input size (for #include expansion)
    local eff_lines="${4:-}"

    local lines bytes
    lines=$(wc -l < "$input")
    bytes=$(wc -c < "$input")
    if [[ -n "$eff_bytes" ]]; then
        bytes="$eff_bytes"
        if [[ -n "$eff_lines" ]]; then
            lines="$eff_lines"
        fi
    fi

    # slow32-fast
    local result
    result=$(run_fast "$input")
    local insn wall outsz
    insn=$(echo "$result" | awk '{print $1}')
    wall=$(echo "$result" | awk '{print $2}')
    outsz=$(echo "$result" | awk '{print $3}')

    # insn/byte
    local insnb="0"
    if [[ "$bytes" -gt 0 && "$insn" -gt 0 ]]; then
        insnb=$(awk "BEGIN { printf \"%.1f\", $insn / $bytes }")
    fi

    # KB/s
    local kbs="0"
    if awk "BEGIN { exit ($wall > 0.001) ? 0 : 1 }"; then
        kbs=$(awk "BEGIN { printf \"%.0f\", ($bytes / 1024) / $wall }")
    fi

    # DBT time
    local tdbt="-"
    if [[ -n "$DBT" ]]; then
        tdbt=$(run_dbt "$input")
    fi

    LABELS+=("$label")
    LINES_ARR+=("$lines")
    BYTES_ARR+=("$bytes")
    OUT_ARR+=("$outsz")
    INSN_ARR+=("$insn")
    INSNB_ARR+=("$insnb")
    KBS_ARR+=("$kbs")
    TFAST_ARR+=("$wall")
    TDBT_ARR+=("$tdbt")
}

echo "=== SLOW-32 Compiler Benchmark ==="
echo ""

# --- Synthetic tests ---
if [[ "$SELF_ONLY" -eq 0 ]]; then
    for sz in $SIZES; do
        label="synth-${sz}K"
        srcfile="$WORKDIR/${label}.c"
        echo -n "  $label ... "
        generate_synthetic "$sz" "$srcfile"
        run_one "$label" "$srcfile"
        echo "done"
    done
fi

# --- Self-compilation ---
# s12cc.c #includes all its headers via the preprocessor — compute effective size
SELF_INCLUDES="c_lexer_gen.c pp.h ast.h parser.h sema.h optimize.h hir.h hir_lower.h hir_ssa.h hir_opt.h hir_licm.h hir_burg.h hir_regalloc.h hir_codegen.h"
SELF_EFF_BYTES=$(wc -c "$SCRIPT_DIR/s12cc.c" $(for f in $SELF_INCLUDES; do echo "$SCRIPT_DIR/$f"; done) 2>/dev/null | tail -1 | awk '{print $1}')
SELF_EFF_LINES=$(wc -l "$SCRIPT_DIR/s12cc.c" $(for f in $SELF_INCLUDES; do echo "$SCRIPT_DIR/$f"; done) 2>/dev/null | tail -1 | awk '{print $1}')
echo -n "  s12cc.c (~${SELF_EFF_BYTES}B effective) ... "
run_one "s12cc.c" "$SCRIPT_DIR/s12cc.c" "$SELF_EFF_BYTES" "$SELF_EFF_LINES"
echo "done"

echo ""

# --- Print table ---
fmt="  %-14s %7s %8s %8s %14s %8s %8s %8s"
printf "$fmt" "Input" "Lines" "Bytes" "Output" "Instructions" "Insn/B" "Time(s)" "KB/s"
if [[ -n "$DBT" ]]; then
    printf " %8s" "DBT(s)"
fi
printf "\n"

printf "$fmt" "--------------" "-------" "--------" "--------" "--------------" "--------" "--------" "--------"
if [[ -n "$DBT" ]]; then
    printf " %8s" "--------"
fi
printf "\n"

i=0
while [[ "$i" -lt "${#LABELS[@]}" ]]; do
    printf "$fmt" \
        "${LABELS[$i]}" \
        "${LINES_ARR[$i]}" \
        "${BYTES_ARR[$i]}" \
        "${OUT_ARR[$i]}" \
        "${INSN_ARR[$i]}" \
        "${INSNB_ARR[$i]}" \
        "${TFAST_ARR[$i]}" \
        "${KBS_ARR[$i]}"
    if [[ -n "$DBT" ]]; then
        printf " %8s" "${TDBT_ARR[$i]}"
    fi
    printf "\n"
    i=$((i + 1))
done

# --- O(n) check ---
# Compute stddev of insn/byte across all entries as % of mean
if [[ "${#INSNB_ARR[@]}" -ge 2 ]]; then
    echo ""
    result=$(awk -v n="${#INSNB_ARR[@]}" 'BEGIN {
        sum = 0; sumsq = 0
    }
    {
        v = $1 + 0
        sum += v
        sumsq += v * v
    }
    END {
        mean = sum / n
        var = sumsq / n - mean * mean
        if (var < 0) var = 0
        sd = sqrt(var)
        pct = (mean > 0) ? 100 * sd / mean : 0
        printf "O(n) check: insn/byte mean=%.1f stddev=%.1f (%.1f%% of mean)\n", mean, sd, pct
    }' <<< "$(printf '%s\n' "${INSNB_ARR[@]}")")
    echo "$result"

    pct=$(echo "$result" | grep -oP '[0-9.]+(?=% of mean)')
    if awk "BEGIN { exit ($pct < 10) ? 0 : 1 }"; then
        echo "  => O(n) CONFIRMED (variation < 10%)"
    else
        echo "  => WARNING: variation >= 10%, scaling may be super-linear"
    fi
fi

echo ""
echo "Done."
