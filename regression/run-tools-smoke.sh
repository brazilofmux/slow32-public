#!/usr/bin/env bash
# Smoke test for the SELF-HOSTED tools: each must actually produce
# output on a real input.
#
# This exists because three of them did not, and nothing noticed.
# slow32dis, slow32dump and every read path of s32-ar shared one bug:
#
#     if (fdseek(f, 0, SEEK_END) != 0) { ...; return 1; }
#
# fdseek has lseek semantics -- it returns the RESULTING OFFSET, not
# fseek's 0-on-success -- so for any non-empty file that test is true
# and the tool bailed out immediately, silently, with no diagnostic.
# They had presumably never worked, and shipped in the kit that way,
# because every other harness in the tree either uses the HOST tools or
# only exercises s32-ar's create path (which does not seek).
#
# The lesson is narrow and cheap to act on: a tool that can fail by
# producing NOTHING needs a test that demands OUTPUT.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
S8="$ROOT/selfhost/stage08"
EMU="${SELFHOST_EMU:-$ROOT/selfhost/stage00/s32-emu}"
W="$(mktemp -d)"
trap 'rm -rf "$W"' EXIT

OBJ="$S8/lib/crt0.s32o"
[ -f "$OBJ" ] || { echo "missing $OBJ" >&2; exit 1; }

pass=0; fail=0
# want_min: the output must have at least this many lines to count as
# "the tool did something", not merely "the tool exited 0".
check() {
    local name="$1" want_min="$2" want_pat="$3"; shift 3
    local out="$W/$name.out"
    if ! timeout 300 "$EMU" "$@" > "$out" 2>"$W/$name.err"; then
        printf "  %-22s FAIL (exit status)\n" "$name"; fail=$((fail+1)); return
    fi
    local n
    n=$(wc -l < "$out")
    if [ "$n" -lt "$want_min" ]; then
        printf "  %-22s FAIL (produced %s lines, expected >= %s)\n" "$name" "$n" "$want_min"
        fail=$((fail+1)); return
    fi
    if ! grep -qE "$want_pat" "$out"; then
        printf "  %-22s FAIL (output missing /%s/)\n" "$name" "$want_pat"
        fail=$((fail+1)); return
    fi
    printf "  %-22s PASS (%s lines)\n" "$name" "$n"; pass=$((pass+1))
}

echo "=== self-hosted tool smoke ==="
check slow32dis  5 "addi|jal|lui"  "$S8/slow32dis.s32x"  "$OBJ"
check slow32dump 4 "s32o|section"  "$S8/slow32dump.s32x" "$OBJ"

# s32-ar: create, then LIST it back -- the read path is the one that broke.
"$EMU" "$S8/s32-ar.s32x" rc "$W/t.s32a" "$S8/lib/ctype.s32o" "$S8/lib/malloc.s32o" >/dev/null 2>&1
if [ -s "$W/t.s32a" ]; then
    check s32-ar-list 2 "ctype|malloc" "$S8/s32-ar.s32x" t "$W/t.s32a"
else
    printf "  %-22s FAIL (create produced nothing)\n" "s32-ar-list"; fail=$((fail+1))
fi

echo ""
echo "tool smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
