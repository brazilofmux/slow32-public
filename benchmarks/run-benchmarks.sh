#!/bin/bash
# Run benchmarks and collect timing/profiling data

cd "$(dirname "$0")"

# Build benchmarks if needed
make -j4 2>/dev/null || { echo "Build failed"; exit 1; }

SLOW32_ROOT=".."
EMU="$SLOW32_ROOT/tools/emulator/slow32"
EMU_FAST="$SLOW32_ROOT/tools/emulator/slow32-fast"
DBT="$SLOW32_ROOT/tools/dbt/slow32-dbt"
TIME_CMD=()
if [ -x /usr/bin/time ]; then
    TIME_CMD=(/usr/bin/time -p)
fi

echo "========================================"
echo "SLOW-32 Benchmark Suite"
echo "========================================"
echo

BENCHMARKS="bench-sum bench-matrix bench-sort bench-fib bench-prime"
SWEEP_THRESHOLDS=${SWEEP_THRESHOLDS:-""}
SWEEP_MIN_SAMPLES=${SWEEP_MIN_SAMPLES:-1000}

for bench in $BENCHMARKS; do
    if [ ! -f "$bench.s32x" ]; then
        echo "Skipping $bench (not built)"
        continue
    fi

    echo "--- $bench ---"

    if [ -x "$EMU" ]; then
        echo -n "  Interpreter: "
        if [ ${#TIME_CMD[@]} -ne 0 ]; then
            out_file=$(mktemp)
            time_out=$( { "${TIME_CMD[@]}" "$EMU" "$bench.s32x" >"$out_file"; } 2>&1 )
            real=$(echo "$time_out" | awk '/^real /{print $2}')
            rm -f "$out_file"
            if [ -n "$real" ]; then
                echo "${real}s"
            else
                echo "$time_out" | head -1
            fi
        else
            "$EMU" "$bench.s32x" >/dev/null 2>&1
            if [ $? -eq 0 ]; then
                echo "ok"
            else
                echo "error"
            fi
        fi
    fi

    # Run with DBT Stage 3 (if available)
    if [ -x "$DBT" ]; then
        echo -n "  DBT Stage 3: "
        if [ ${#TIME_CMD[@]} -ne 0 ]; then
            out_file=$(mktemp)
            time_out=$( { "${TIME_CMD[@]}" "$DBT" -3 "$bench.s32x" >"$out_file"; } 2>&1 )
            real=$(echo "$time_out" | awk '/^real /{print $2}')
            rm -f "$out_file"
            if [ -n "$real" ]; then
                echo "${real}s"
            else
                echo "$time_out" | head -1
            fi
        else
            "$DBT" -3 "$bench.s32x" >/dev/null 2>&1
            if [ $? -eq 0 ]; then
                echo "ok"
            else
                echo "error"
            fi
        fi

        echo -n "  DBT Stage 4: "
        if [ ${#TIME_CMD[@]} -ne 0 ]; then
            out_file=$(mktemp)
            time_out=$( { "${TIME_CMD[@]}" "$DBT" -4 "$bench.s32x" >"$out_file"; } 2>&1 )
            real=$(echo "$time_out" | awk '/^real /{print $2}')
            rm -f "$out_file"
            if [ -n "$real" ]; then
                echo "${real}s"
            else
                echo "$time_out" | head -1
            fi
        else
            "$DBT" -4 "$bench.s32x" >/dev/null 2>&1
            if [ $? -eq 0 ]; then
                echo "ok"
            else
                echo "error"
            fi
        fi

        echo -n "  DBT Stage 4 (no regcache): "
        if [ ${#TIME_CMD[@]} -ne 0 ]; then
            out_file=$(mktemp)
            time_out=$( { "${TIME_CMD[@]}" "$DBT" -4 -R "$bench.s32x" >"$out_file"; } 2>&1 )
            real=$(echo "$time_out" | awk '/^real /{print $2}')
            rm -f "$out_file"
            if [ -n "$real" ]; then
                echo "${real}s"
            else
                echo "$time_out" | head -1
            fi
        else
            "$DBT" -4 -R "$bench.s32x" >/dev/null 2>&1
            if [ $? -eq 0 ]; then
                echo "ok"
            else
                echo "error"
            fi
        fi

    fi

    echo
done

if [ -n "$SWEEP_THRESHOLDS" ] && [ -x "$DBT" ]; then
    echo "========================================"
    echo "Threshold Sweep (Stage 4, two-pass)"
    echo "========================================"
    echo
    for bench in $BENCHMARKS; do
        if [ ! -f "$bench.s32x" ]; then
            continue
        fi
        echo "--- $bench ---"
        for t in $SWEEP_THRESHOLDS; do
            echo -n "  T=${t}%: "
            result=$($DBT -4 -t -s -M "$SWEEP_MIN_SAMPLES" -T "$t" "$bench.s32x" 2>&1 | awk '/^Time: /{print $2\"s\"}')
            if [ -n "$result" ]; then
                echo "$result"
            else
                echo "error"
            fi
        done
        echo
    done
fi

echo "========================================"
echo "Detailed profiling (with -s flag)"
echo "========================================"
echo

# If DBT supports profiling, use it
if [ -x "$DBT" ]; then
    for bench in $BENCHMARKS; do
        if [ ! -f "$bench.s32x" ]; then
            continue
        fi

        echo "--- $bench profiling ---"
        $DBT -4 -s "$bench.s32x" 2>&1 | grep -E "(blocks|cache|chain|exit|inst)" || true
        echo
    done
fi
