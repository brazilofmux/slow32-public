#!/usr/bin/env bash
# layout-sweep.sh — measure a build across a range of code-layout offsets,
# to separate a code change's true dynamic effect from layout luck.
#
# Background: on out-of-order x86 (esp. Cascade Lake), the absolute alignment
# of hot code mod 16/32/64 swings benchmark time by ~10%+ via DSB packing,
# the Jcc erratum, and fetch alignment. Any single A/B of a code change is
# therefore contaminated: the change shifts addresses and re-rolls that dice.
# This harness rebuilds the emulator at several fixed code offsets (via the
# cc S32_TEXT_PAD knob — N dead bytes at the start of .text) and reports the
# time distribution. Run it for baseline and for a change; compare the
# DISTRIBUTIONS (min/median across offsets), not single points.
#
# Usage:
#   layout-sweep.sh <cross-dir> [pad-list]
#     <cross-dir>  stage08-cross-x64 | stage08-cross-a64 (relative to selfhost/)
#   env:
#     RUN_MODE=native   (default) time ./out/s32fast-hir directly  (Kagura/x86 bare metal)
#     RUN_MODE=podman-a64        run the arm64 ELF under podman w/ GNU date  (local M-series)
#     BENCH=<path>      benchmark .s32x (default ~/s32x/benchmark_core.s32x)
#     REPS=<n>          timed reps per offset, min taken (default 7)
#     PADS="0 16 ..."   override the offset list
#
# Each offset rebuilds ONLY the emulator object (libc/crt0 stay fixed), so
# the only thing that moves is the hot code's absolute alignment.
set -u

SELFHOST="$(cd "$(dirname "$0")/.." && pwd)"
DIR="${1:?usage: layout-sweep.sh <cross-dir> [pads]}"
CD="$SELFHOST/$DIR"
BENCH="${BENCH:-$HOME/s32x/benchmark_core.s32x}"
REPS="${REPS:-7}"
RUN_MODE="${RUN_MODE:-native}"
PADS="${2:-${PADS:-0 16 32 48 64 80 96 112 128 144 160 176 192 208 224 240}}"

# Per-tree object/binary/compiler target names.
case "$DIR" in
  *x64) OBJ="out/s32fast-hir.o"; BIN="out/s32fast-hir"; MK="out/s32fast-hir"; CCBIN="out/cc-x64" ;;
  *a64) OBJ="out/s32fast.o";     BIN="out/s32fast-hir"; MK="s32fast";         CCBIN="out/cc-a64" ;;
  *) echo "unknown tree: $DIR"; exit 1 ;;
esac

[ -f "$BENCH" ] || { echo "benchmark not found: $BENCH"; exit 1; }
cd "$CD" || exit 1
# Build only the compiler (the default 'make' on the a64 tree runs ELF tests
# that can't exec on a non-Linux host).
make "$CCBIN" >/dev/null 2>&1 || { echo "compiler build failed"; exit 1; }

# time_run: echo the min wall-time (seconds, float) over $REPS executions.
time_run() {
  if [ "$RUN_MODE" = "podman-a64" ]; then
    cp "$BENCH" out/_bench.s32x
    # GNU date (%s%N → integer ns) lives in debian; pure-bash min, no bc/awk.
    # Container starts once; the per-exec loop excludes container startup.
    podman run --rm --platform linux/arm64 -v "$PWD/out":/o:ro \
      docker.io/library/debian:stable-slim bash -c '
        b=/o/_bench.s32x; min=0
        for i in $(seq '"$REPS"'); do
          s=$(date +%s%N); /o/s32fast-hir "$b" >/dev/null 2>&1; e=$(date +%s%N)
          d=$((e - s))
          if [ $min -eq 0 ] || [ $d -lt $min ]; then min=$d; fi
        done
        printf "%d.%03d\n" $((min/1000000000)) $(((min/1000000)%1000))'
    rm -f out/_bench.s32x
  else
    local min=999 d
    for _ in $(seq "$REPS"); do
      d=$( { /usr/bin/time -f %e taskset -c 1 "./$BIN" "$BENCH" >/dev/null 2>/dev/null; } 2>&1 )
      awk "BEGIN{exit !($d < $min)}" && min=$d
    done
    echo "$min"
  fi
}

echo "# layout sweep: $DIR  REPS=$REPS  mode=$RUN_MODE"
echo "# pad(bytes)  min_time(s)"
best=999; worst=0
for pad in $PADS; do
  rm -f "$OBJ"
  S32_TEXT_PAD="$pad" make "$MK" >/dev/null 2>&1 || { echo "$pad  BUILD-FAIL"; continue; }
  t=$(time_run)
  printf "%6s      %s\n" "$pad" "$t"
  awk "BEGIN{exit !($t < $best)}" && best=$t
  awk "BEGIN{exit !($t > $worst)}" && worst=$t
done
# restore an unpadded build
rm -f "$OBJ"; make "$MK" >/dev/null 2>&1
spread=$(awk "BEGIN{printf \"%.1f\", ($worst/$best - 1)*100}")
echo "# best=$best worst=$worst  layout spread=${spread}%"
