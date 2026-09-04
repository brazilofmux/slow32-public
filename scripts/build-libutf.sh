#!/usr/bin/env bash
# build-libutf.sh -- build libutf (~/utf, Unicode 16) for SLOW-32 with the
# stage08 compiler, link its examples and its 503-check stress harness, and
# with --check run them against the host build.
#
# The harness is the sharpest compiler stress oracle this tree has: on
# 2026-09-03 it surfaced five stage08 defects and one runtime defect in an
# afternoon (selfhost ISSUES-65, runtime ISSUES-13). Run it after any
# front-end / hir_opt change.
#
# Usage:  scripts/build-libutf.sh [--check] [--out DIR]
#   UTF_DIR   libutf checkout            (default ~/utf)
#   CC08      stage08 compiler           (default selfhost/stage08/cc.s32x)
#   EMU       emulator that runs cc.s32x (default tools/dbt/slow32-dbt)
#   --check   needs the host build: `make -C $UTF_DIR test examples`
#
# Outputs in DIR (default build/libutf): libutf.s32a, nfc_check.s32x,
# sort_lines.s32x, strip_color.s32x, nearest_color.s32x, test_color_ops.s32x.
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
UTF_DIR="${UTF_DIR:-$HOME/utf}"
CC08="${CC08:-$ROOT/selfhost/stage08/cc.s32x}"
EMU="${EMU:-$ROOT/tools/dbt/slow32-dbt}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
OUT="$ROOT/build/libutf"
CHECK=0
while [ $# -gt 0 ]; do
    case "$1" in
        --check) CHECK=1 ;;
        --out) OUT="$2"; shift ;;
        *) echo "usage: $0 [--check] [--out DIR]" >&2; exit 2 ;;
    esac
    shift
done
for f in "$CC08" "$EMU" "$AS" "$LD" "$UTF_DIR/Makefile"; do
    [ -e "$f" ] || { echo "missing: $f" >&2; exit 1; }
done
mkdir -p "$OUT"

# The library is LIB_SRCS in libutf's Makefile: 7 units in src/ and 8 table
# units in tables/. src/*.c alone is NOT the library.
SRCS=$(sed -n '/^LIB_SRCS/,/[^\\]$/p' "$UTF_DIR/Makefile" | grep -oE '[A-Za-z0-9_/.-]+\.c')
INC="-I $UTF_DIR/include -I $UTF_DIR/src -I $ROOT/runtime/include"

# The emulator prints a banner around guest output; strip it for diffs.
STRIP='^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code|Instructions executed|Simulated cycles|Cycles:|Wall time|Performance|.*instructions/second|.*intrinsics detected|memcpy:|hir_)'

cc08() {  # cc08 SRC OUT.s  (stage08 cc under the emulator; fails if no output)
    rm -f "$2"
    "$EMU" "$CC08" $INC "$1" "$2" > "$2.log" 2>&1
    if [ ! -s "$2" ]; then
        echo "compile failed: $1" >&2; grep -m3 -E 'expected|error' "$2.log" >&2; return 1
    fi
}
link() {  # link OBJ EXE  -- builtins64 for __muldi3; the harness needs > 64K stack
    "$LD" -o "$2" --mmio 64K --stack-size 1M "$ROOT/runtime/crt0.s32o" "$1" \
        "$OUT/libutf.s32a" "$ROOT/selfhost/stage08/lib/builtins64.s32o" \
        "$ROOT/runtime/libc_mmio.s32a" "$ROOT/runtime/libs32.s32a"
}

echo "== libutf: $(echo "$SRCS" | wc -l | tr -d ' ') units"
rm -f "$OUT"/lib_*.s32o "$OUT/libutf.s32a"
fail=0
for rel in $SRCS; do
    b="lib_$(echo "$rel" | tr '/' '_' | sed 's/\.c$//')"
    cc08 "$UTF_DIR/$rel" "$OUT/$b.s" || { fail=1; continue; }
    "$AS" "$OUT/$b.s" "$OUT/$b.s32o" >/dev/null || { echo "assemble failed: $rel" >&2; fail=1; }
done
[ $fail -eq 0 ] || exit 1
"$ROOT/tools/utilities/s32-ar" rc "$OUT/libutf.s32a" "$OUT"/lib_*.s32o
echo "   $OUT/libutf.s32a ($(wc -c < "$OUT/libutf.s32a" | tr -d ' ') bytes)"

echo "== examples + harness"
for src in "$UTF_DIR"/examples/*.c "$UTF_DIR/tests/test_color_ops.c"; do
    n="$(basename "$src" .c)"
    cc08 "$src" "$OUT/$n.s" || { fail=1; continue; }
    "$AS" "$OUT/$n.s" "$OUT/$n.s32o" >/dev/null || { fail=1; continue; }
    link "$OUT/$n.s32o" "$OUT/$n.s32x" || { fail=1; continue; }
    echo "   $n.s32x"
done
[ $fail -eq 0 ] || exit 1
[ $CHECK -eq 1 ] || exit 0

echo "== check against the host build"
H="$UTF_DIR"
for f in "$H/tests/test_color_ops" "$H/examples/sort_lines" "$H/examples/strip_color" "$H/examples/nearest_color"; do
    [ -x "$f" ] || { echo "host binary missing: $f (make -C $UTF_DIR test examples)" >&2; exit 1; }
done
guest() { "$EMU" "$1" "${@:2}" 2>&1 | grep -Ev "$STRIP"; }
bad=0
verdict() {  # verdict NAME HOSTFILE GUESTFILE
    if cmp -s "$2" "$3"; then echo "   $1: identical"; else echo "   $1: DIFFERS"; diff "$2" "$3" | head -5; bad=1; fi
}
"$H/tests/test_color_ops" -s 12345 > "$OUT/h_harness.txt" 2>&1
guest "$OUT/test_color_ops.s32x" -s 12345 > "$OUT/g_harness.txt" < /dev/null
verdict "harness -s 12345 ($(grep -c . "$OUT/h_harness.txt") lines)" "$OUT/h_harness.txt" "$OUT/g_harness.txt"

printf 'pear\napple\n\xc3\x85ngstr\xc3\xb6m\nangstrom\nzebra\ncaf\xc3\xa9\ncafe\n\xe4\xb8\x96\xe7\x95\x8c\nBanana\n' > "$OUT/in_sort.txt"
"$H/examples/sort_lines" < "$OUT/in_sort.txt" > "$OUT/h_sort.txt" 2>&1
guest "$OUT/sort_lines.s32x" < "$OUT/in_sort.txt" > "$OUT/g_sort.txt"
verdict "sort_lines" "$OUT/h_sort.txt" "$OUT/g_sort.txt"

printf '\033[31mHello\033[0m \033[1;32mWorld\033[0m caf\xc3\xa9 \033[38;5;208mor\xc3\xa1nge\033[0m\n' > "$OUT/in_color.txt"
for t in ascii ansi16 ansi256 truecolor; do
    "$H/examples/strip_color" "$t" < "$OUT/in_color.txt" > "$OUT/h_strip_$t.txt" 2>&1
    guest "$OUT/strip_color.s32x" "$t" < "$OUT/in_color.txt" > "$OUT/g_strip_$t.txt"
    verdict "strip_color $t" "$OUT/h_strip_$t.txt" "$OUT/g_strip_$t.txt"
done

: > "$OUT/h_near.txt"; : > "$OUT/g_near.txt"
for rgb in "0 0 0" "255 255 255" "128 64 32" "12 200 99" "255 0 0" "0 128 255" "77 77 77" "250 128 114"; do
    "$H/examples/nearest_color" $rgb >> "$OUT/h_near.txt" 2>&1
    guest "$OUT/nearest_color.s32x" $rgb >> "$OUT/g_near.txt" < /dev/null
done
verdict "nearest_color (8 probes)" "$OUT/h_near.txt" "$OUT/g_near.txt"

[ $bad -eq 0 ] && echo "== libutf on SLOW-32: all identical to the host" || { echo "== libutf on SLOW-32: DIVERGENCE"; exit 1; }
