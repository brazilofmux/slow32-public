#!/usr/bin/env bash
# gcc-layout-sweep.sh — the GCC counterpart to layout-sweep.sh.
#
# Question it answers (IMPROVEMENTS.md "clever or lucky?"): we showed cc-x64's
# OWN best alignment phase hits 1.08s == gcc's single slow32-fast build. But
# that compared our sweep BEST to gcc's ONE build. To know whether gcc is
# robustly well-aligned (clever) or just rolled a good build (lucky), sweep
# gcc's slow32-fast across the SAME kind of layout offsets and compare its
# spread/median to ours:
#   - gcc spread ~= ours (~20%)  -> gcc got lucky; cc-x64 genuinely matches it.
#   - gcc spread << ours         -> gcc has real alignment robustness
#                                   (likely -falign-* + Jcc-erratum padding).
#
# Mechanism: gcc builds slow32-fast in one shot. We prepend a pad object whose
# .text is N bytes, so gcc's code shifts by N (rounded to the 16B function
# alignment) — the uniform-shift analogue of cc-x64's S32_TEXT_PAD knob. This
# perturbs absolute alignment while keeping gcc's relative layout + codegen
# identical.
#
# Usage:  gcc-layout-sweep.sh [pad-list]
#   env:
#     RUN_MODE=native     (default) time the binary directly (Kagura/x86 bare metal)
#     RUN_MODE=podman-amd64         build+checksum under a gcc container (local
#                                   validation only — qemu timing is meaningless)
#     BENCH=<path>        benchmark .s32x (default ~/s32x/benchmark_core.s32x)
#     REPS=<n>            timed reps per offset, min taken (default 7)
#     EMU=<dir>           tools/emulator dir (default: repo tools/emulator)
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
EMU="${EMU:-$REPO/tools/emulator}"
BENCH="${BENCH:-$HOME/s32x/benchmark_core.s32x}"
REPS="${REPS:-7}"
RUN_MODE="${RUN_MODE:-native}"
PADS="${1:-${PADS:-0 16 32 48 64 80 96 112 128 144 160 176 192 208 224 240}}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

CC="${CC:-gcc}"
CFLAGS="-Wall -Wextra -O3 -fomit-frame-pointer -g"

[ -f "$EMU/slow32-fast.c" ] || { echo "slow32-fast.c not found in $EMU"; exit 1; }
[ -f "$BENCH" ] || { echo "benchmark not found: $BENCH"; exit 1; }

# build_padded <N> <outbin>: gcc-build slow32-fast with N bytes prepended to .text
build_padded() {
  local n="$1" out="$2"
  # CRITICAL: the pad must go in .text.startup, NOT plain .text. gcc -O3
  # -freorder-functions splits code into .text.startup (incl. main) / .text.hot
  # / plain .text. A plain-.text pad lands AFTER main (~0x4c59) and shifts only
  # the trailing handlers — main and the INLINED hot dispatch loop never move,
  # so the sweep only proves handler robustness, not loop robustness (it read
  # ~2.9% that way). The dispatch loop (cpu_step_fast inlined into main) is the
  # DSB-packing-sensitive code our knob perturbs. .text.startup leads that
  # bucket, so the pad shifts main+loop AND the trailing handlers — the true
  # whole-blob analogue of cc-x64's S32_TEXT_PAD. (Verified: main moves
  # 0x24b0→0x25a0 across pads; spread collapses to 1.0% = gcc is robust.)
  printf '\t.section .text.startup,"ax",@progbits\n\t.globl __s32_layout_pad\n__s32_layout_pad:\n\t.space %d\n' "$n" > "$WORK/pad.s"
  "$CC" -c "$WORK/pad.s" -o "$WORK/pad.o" 2>/dev/null || return 1
  # pad.o first → its .text.startup leads → main + everything after shift by ~N.
  "$CC" $CFLAGS -o "$out" "$WORK/pad.o" "$EMU/slow32-fast.c" "$EMU/mmio_ring.c" \
    -I"$EMU" -lm 2>/dev/null
}

# time_run <bin>: echo min wall-time (s) over REPS, native or under podman.
time_run() {
  local bin="$1"
  if [ "$RUN_MODE" = "podman-amd64" ]; then
    cp "$bin" "$WORK/_b"; cp "$BENCH" "$WORK/_bench.s32x"
    podman run --rm --platform linux/amd64 -v "$WORK":/o:ro \
      docker.io/library/debian:stable-slim bash -c '
        min=0
        for i in $(seq '"$REPS"'); do
          s=$(date +%s%N); /o/_b /o/_bench.s32x >/dev/null 2>&1; e=$(date +%s%N)
          d=$((e - s)); if [ $min -eq 0 ] || [ $d -lt $min ]; then min=$d; fi
        done
        printf "%d.%03d\n" $((min/1000000000)) $(((min/1000000)%1000))'
  else
    local min=999 d tf; tf="$WORK/_t"
    for _ in $(seq "$REPS"); do
      /usr/bin/time -f %e -o "$tf" taskset -c 1 "$bin" "$BENCH" >/dev/null 2>&1
      d=$(cat "$tf")
      [ -n "$d" ] && awk "BEGIN{exit !($d < $min)}" && min=$d
    done
    echo "$min"
  fi
}

# checksum <bin>: echo the emulator's reported checksum (correctness guard).
checksum() {
  local bin="$1"
  if [ "$RUN_MODE" = "podman-amd64" ]; then
    cp "$bin" "$WORK/_b"; cp "$BENCH" "$WORK/_bench.s32x"
    podman run --rm --platform linux/amd64 -v "$WORK":/o:ro \
      docker.io/library/debian:stable-slim sh -c '/o/_b /o/_bench.s32x 2>&1 | grep -i checksum'
  else
    "$bin" "$BENCH" 2>&1 | grep -i checksum
  fi
}

echo "# gcc layout sweep: $EMU/slow32-fast.c  REPS=$REPS  mode=$RUN_MODE  CC=$CC"
echo "# pad(bytes)  min_time(s)   .text(h_? base via nm main)"
best=999; worst=0; first_csum=""
for pad in $PADS; do
  bin="$WORK/sf_$pad"
  build_padded "$pad" "$bin" || { echo "$pad  BUILD-FAIL"; continue; }
  # correctness: checksum must be identical across all offsets
  cs=$(checksum "$bin" | tr -dc '0-9a-fx')
  [ -z "$first_csum" ] && first_csum="$cs"
  [ "$cs" = "$first_csum" ] || echo "  !! checksum drift at pad=$pad: $cs vs $first_csum"
  base=$(nm "$bin" 2>/dev/null | awk '$3=="main"{print $1}')
  t=$(time_run "$bin")
  printf "%6s      %-9s    main@0x%s\n" "$pad" "$t" "$base"
  awk "BEGIN{exit !($t < $best)}" && best=$t
  awk "BEGIN{exit !($t > $worst)}" && worst=$t
done
spread=$(awk "BEGIN{ if ($best>0) printf \"%.1f\", ($worst/$best - 1)*100; else print \"n/a\" }")
echo "# best=$best worst=$worst  gcc layout spread=${spread}%  (checksum $first_csum)"
echo "# Compare this spread/median to layout-sweep.sh stage08-cross-x64 (ours)."
