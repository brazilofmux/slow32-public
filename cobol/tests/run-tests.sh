#!/bin/bash
# cobol/ test harness.
#
# Gate 1 (pictest): pic_analyse over tests/pictures.txt against an expected
#   file checked by hand against the 1985 PICTURE clause text.
# Gate 2 (programs): every tests/fixed/*.cbl and tests/free/*.cbl compiled by
#   s32-cobc, assembled, linked with libcob and the SLOW-32 libc, run on the
#   emulator; stdout must match the .expected file.  When GnuCOBOL's cobc is
#   on the host, the same source is also run under it and diffed, so the
#   .expected files are checked against the oracle as well as against us
#   (docs/oracles.md: -std=cobol85 for portable programs; a program whose
#   first comment names "default dialect" uses GnuCOBOL's default, because
#   it exercises implementor usages -std=cobol85 rejects).
# Gate 3 (refusals): every tests/bad/*.cbl must be refused, and the message
#   must contain the text in its .expected file.  Unimplemented is a
#   diagnostic, never silence.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
COBC="$CDIR/out/s32-cobc"
LIBCOB="$CDIR/libcob/libcob.s32o"
ORACLE="$(command -v cobc || true)"

W="$(mktemp -d "${TMPDIR:-/tmp}/cobol-tests.XXXXXX")"
trap 'rm -rf "$W"' EXIT

PASS=0; FAIL=0

report() {
    if [ "$2" = "0" ]; then printf "  %-28s PASS%s\n" "$1:" "${3:+ ($3)}"; PASS=$((PASS+1))
    else printf "  %-28s FAIL%s\n" "$1:" "${3:+ ($3)}"; FAIL=$((FAIL+1)); fi
}

emu_run() {   # emu_run prog.s32x > stdout: the guest's output only
    # Everything between the emulator's "Starting execution" line and its
    # halt report is the program's.  Blank lines inside are the program's
    # too (DISPLAY of nothing), so this is a capture, not a grep -v.
    "$EMU" "$1" 2>/dev/null | awk '
        /^Starting execution/ { capture = 1; next }
        /^HALT at|^Program halted|^Exit code/ { capture = 0 }
        capture { print }'
}

if [ ! -x "$COBC" ] || [ ! -f "$LIBCOB" ]; then
    "$CDIR/build.sh" >/dev/null || { echo "build failed"; exit 1; }
fi

# --- Gate 1: PICTURE ---------------------------------------------------
if ! cc -std=c99 -I"$CDIR/src" -O1 -w -o "$W/pictest" "$HERE/pictest.c" \
        "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" 2>"$W/cc.log"; then
    report "pictest" 1 "host build"
else
    "$W/pictest" "$HERE/pictures.txt" > "$W/pictures.out" 2>&1
    if diff -q "$W/pictures.out" "$HERE/pictures.expected" >/dev/null 2>&1; then
        report "pictest" 0
    else
        report "pictest" 1 "mismatch"
        diff "$HERE/pictures.expected" "$W/pictures.out" | head -12
    fi
fi

# --- Gate 2: programs --------------------------------------------------
for fmt in fixed free; do
    for src in "$HERE/$fmt"/*.cbl; do
        [ -e "$src" ] || continue
        name="$(basename "$src" .cbl)"
        exp="${src%.cbl}.expected"
        flag="-$fmt"
        if ! "$COBC" $flag -o "$W/$name.s" "$src" 2>"$W/$name.err"; then
            report "$fmt/$name" 1 "$(head -1 "$W/$name.err")"; continue
        fi
        if ! "$AS" "$W/$name.s" "$W/$name.s32o" >"$W/$name.as" 2>&1; then
            report "$fmt/$name" 1 "assemble: $(grep -m1 -i error "$W/$name.as")"; continue
        fi
        if ! "$LD" -o "$W/$name.s32x" "$ROOT/runtime/crt0.s32o" "$W/$name.s32o" "$LIBCOB" \
                "$ROOT/runtime/libc_debug.s32a" "$ROOT/runtime/libs32.s32a" >"$W/$name.ld" 2>&1; then
            report "$fmt/$name" 1 "link: $(grep -m1 -i error "$W/$name.ld")"; continue
        fi
        emu_run "$W/$name.s32x" > "$W/$name.out"
        if [ ! -f "$exp" ]; then
            report "$fmt/$name" 1 "no .expected file"; continue
        fi
        if ! diff -q "$W/$name.out" "$exp" >/dev/null; then
            report "$fmt/$name" 1 "output mismatch"
            diff "$exp" "$W/$name.out" | head -8
            continue
        fi
        # oracle: GnuCOBOL on the same source, when present
        note=""
        if [ -n "$ORACLE" ]; then
            std="-std=cobol85"
            grep -qi "default dialect" "$src" && std=""
            if "$ORACLE" -x $std $flag -o "$W/$name.orc" "$src" >"$W/$name.orclog" 2>&1; then
                "$W/$name.orc" > "$W/$name.orcout" 2>/dev/null
                if diff -q "$W/$name.orcout" "$exp" >/dev/null; then note="oracle agrees"
                else
                    report "$fmt/$name" 1 "GnuCOBOL disagrees with .expected"
                    diff "$exp" "$W/$name.orcout" | head -8
                    continue
                fi
            else
                note="oracle refused it: $(grep -m1 error "$W/$name.orclog" | cut -c1-60)"
            fi
        fi
        report "$fmt/$name" 0 "$note"
    done
done

# --- Gate 3: refusals --------------------------------------------------
for src in "$HERE/bad"/*.cbl; do
    [ -e "$src" ] || continue
    name="$(basename "$src" .cbl)"
    exp="${src%.cbl}.expected"
    flag="-fixed"; grep -q "^identification division" "$src" && flag="-free"
    [ "$name" = "mixed-format" ] && flag="-fixed"
    if "$COBC" $flag -o "$W/$name.s" "$src" 2>"$W/$name.err"; then
        report "bad/$name" 1 "was accepted"; continue
    fi
    if grep -qF "$(cat "$exp")" "$W/$name.err"; then
        report "bad/$name" 0 "$(cut -d: -f3- "$W/$name.err" | head -1 | cut -c1-50)"
    else
        report "bad/$name" 1 "wrong message: $(head -1 "$W/$name.err")"
    fi
done

echo
echo "cobol: $PASS passed, $FAIL failed"
[ "$FAIL" = "0" ]
