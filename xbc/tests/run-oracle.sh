#!/bin/bash
# The interpreter is the oracle: every program in the corpus is run by
# xbase.s32x (SET TALK OFF / DO name / QUIT, as a compiled program starts
# with TALK off) and compiled by s32-xbc, each in a fresh copy of its
# directory, and the two outputs must be the same bytes.
#
#   tests/run-oracle.sh             # the whole corpus
#   tests/run-oracle.sh FOO.PRG ... # some programs (paths)
#
# A program whose data a xbase test script makes (CREATE, APPEND ...) before
# it DOes the program gets that script's lines up to the DO run first, in
# both copies, output discarded.
#
# Verdicts: AGREE; DIFFER (a gate failure: diff kept in out/oracle/);
# NOCOMPILE (s32-xbc refuses it -- the progress measure, not a failure);
# SKIP (the program reads the keyboard, which this harness does not feed).
# Exit status 1 when anything DIFFERs.
#
# Programs compile through compile-prg.sh, whose environment picks the
# compiler (XBC_CC=stage08 has stage08 cc compile the generated C against
# the clang-built runtime: a calling-convention gate as much as a
# compiler one).  An installed copy runs it with
#   XBASE      the interpreter (default ../xbase/xbase.s32x)
#   XBC_COMPILE the compile command, taking prog.prg out.s32x
#              (default: this tree's compile-prg.sh; the image's is s32xbc)
#   EMU        the engine both run on
set -u

CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
DBASE="${XBASE:-$ROOT/xbase/xbase.s32x}"
OUT="$CLIP_DIR/out/oracle"
mkdir -p "$OUT"

if [ -z "${XBC_COMPILE:-}" ]; then
    (cd "$CLIP_DIR" && bash build.sh >/dev/null) || { echo "s32-xbc build failed"; exit 2; }
    compile() { bash "$CLIP_DIR/compile-prg.sh" "$@"; }
else
    compile() { $XBC_COMPILE "$@"; }
fi

if [ $# -gt 0 ]; then
    CORPUS=("$@")
else
    CORPUS=("$CLIP_DIR"/tests/*.prg "$ROOT"/xbase/tests/*.PRG)
fi

# Byte work in the shell's own tools: the slow32:xbase image, where this
# also runs, carries no python.
#
# rstrip_nl FILE: its bytes without trailing newlines
rstrip_nl() {
    local n; n=$(($(wc -c < "$1")))      # macOS pads the count with spaces
    while [ "$n" -gt 0 ] &&
          [ "$(tail -c +"$n" "$1" | head -c 1 | od -An -c | tr -d ' ')" = '\n' ]; do
        n=$((n - 1))
    done
    if [ "$n" -gt 0 ]; then head -c "$n" "$1"; fi      # macOS head refuses -c 0
}

# Banner (3 lines), then the prompts ". " for SET TALK OFF and DO, the
# program's output, and the prompt for QUIT: strip exactly those.  ? starts
# its line, so the program may end mid-line, and the QUIT prompt then
# starts one of its own: trailing newlines are not compared (either side).
strip_interp() {
    local t u n
    t=$(mktemp "$OUT/s.XXXXXX"); u=$(mktemp "$OUT/s.XXXXXX")
    cat > "$t"
    tail -n +4 "$t" > "$u"
    if [ "$(head -c 4 "$u")" = ". . " ]; then tail -c +5 "$u" > "$t"; else cat "$u" > "$t"; fi
    n=$(($(wc -c < "$t")))
    if [ "$n" -ge 2 ] && [ "$(tail -c 2 "$t")" = ". " ]; then
        if [ "$n" -gt 2 ]; then head -c $((n - 2)) "$t" > "$u"; else : > "$u"; fi
    else
        cat "$t" > "$u"
    fi
    rstrip_nl "$u"
    rm -f "${t:?}" "${u:?}"
}

# The lines of the first xbase test script in dir that DOes name, up to it
setup_for() {
    local dir=$1 name=$2 t
    for t in "$dir"/*.txt; do
        [ -f "$t" ] || continue
        if grep -qiE "^[[:space:]]*DO[[:space:]]+$name(\.PRG)?[[:space:]]*\$" "$t"; then
            awk -v n="$name" 'BEGIN{IGNORECASE=1} toupper($0) ~ "^[ \t]*DO[ \t]+" toupper(n) "(\\.PRG)?[ \t]*$" {exit} {print}' "$t"
            return
        fi
    done
}

# The keys a program that reads the keyboard is given: NAME.keys beside
# it, else the lines an xbase test script types after DOing it (up to its
# QUIT).  Under --deny term the interpreter reads them from its command
# stream after the DO line and the compiled program from its stdin; a key
# a program leaves unread becomes an interpreter command, so that differs
# too.  Empty, with status 1, when there are none.
keys_for() {
    local prg=$1 dir=$2 name=$3 t
    if [ -f "${prg%.*}.keys" ]; then cat "${prg%.*}.keys"; return 0; fi
    for t in "$dir"/*.txt; do
        [ -f "$t" ] || continue
        if grep -qiE "^[[:space:]]*DO[[:space:]]+$name(\.PRG)?[[:space:]]*\$" "$t"; then
            awk -v n="$name" 'BEGIN{IGNORECASE=1} on && toupper($0) ~ "^[ \t]*QUIT[ \t]*$" {exit}
                on {print} toupper($0) ~ "^[ \t]*DO[ \t]+" toupper(n) "(\\.PRG)?[ \t]*$" {on=1}' "$t"
            return 0
        fi
    done
    return 1
}

# A compiled program stops at an error no handler of its own answers, as
# Clipper's does (the default handler prints "Error BASE/1020 ..." and the
# calls, and quits); the interpreter reports it and goes on, as dBASE does
# (user ruling, 2026-10-30).  Such a pair agrees "to its error" when the
# two print the same up to it -- the interpreter's "*** " report, the
# compiled program's Alert -- and the compiled one ends with its calls.
upto_error() {   # upto_error INTERP CLIP: 0 when they agree to the first error
    local i c ni nc t
    ni=$(grep -b -o -m1 '\*\*\* ' "$1" | head -1 | cut -d: -f1)
    nc=$(grep -b -o -m1 -E 'Error [A-Za-z]+/[0-9]+  ' "$2" | head -1 | cut -d: -f1)
    [ -n "$ni" ] && [ -n "$nc" ] || return 1
    grep -q '^Called from ' "$2" || return 1
    t=$(mktemp "$OUT/u.XXXXXX")
    if [ "$ni" -gt 0 ]; then head -c "$ni" "$1" > "$t"; else : > "$t"; fi
    i=$(rstrip_nl "$t")
    if [ "$nc" -gt 0 ]; then head -c "$nc" "$2" > "$t"; else : > "$t"; fi
    c=$(rstrip_nl "$t")
    rm -f "${t:?}"
    [ "$i" = "$c" ]
}

# A program in tests/oracle-stops.txt: the compiled one stopped with the
# default handler's report, and what it printed before is how the
# interpreter's output begins
stops_as_listed() {   # stops_as_listed NAME INTERP CLIP
    local nc t c
    grep -qE "^$1([[:space:]]|\$)" "$CLIP_DIR/tests/oracle-stops.txt" 2>/dev/null || return 1
    nc=$(grep -b -o -m1 -E 'Error [A-Za-z]+/[0-9]+  ' "$3" | head -1 | cut -d: -f1)
    [ -n "$nc" ] && grep -q '^Called from ' "$3" || return 1
    t=$(mktemp "$OUT/u.XXXXXX")
    if [ "$nc" -gt 0 ]; then head -c "$nc" "$3" > "$t"; else : > "$t"; fi
    c=$(rstrip_nl "$t")
    rm -f "${t:?}"
    [ "$(head -c ${#c} "$2")" = "$c" ]
}

agree=0; differ=0; nocomp=0; skip=0
for prg in "${CORPUS[@]}"; do
    name=$(basename "$prg"); name="${name%.*}"
    dir=$(cd "$(dirname "$prg")" && pwd)
    first=$(grep -viE '^[[:space:]]*(\*|&&|NOTE\b|$)' "$prg" | head -1)
    if echo "$first" | grep -qiE '^[[:space:]]*(FUNCTION|PROCEDURE)\b'; then
        printf '%-28s SKIP (a library: reached through its caller)\n' "$name"; skip=$((skip+1)); continue
    fi
    haskeys=0
    if grep -qiE '^[[:space:]]*(ACCEPT|INPUT|WAIT|READ)\b|INKEY\(' "$prg"; then
        # a file, not a variable: $(...) would drop a last empty line
        if ! keys_for "$prg" "$dir" "$name" > "$OUT/$name.keysin"; then
            printf '%-28s SKIP (reads the keyboard; no keys for it)\n' "$name"; skip=$((skip+1)); continue
        fi
        haskeys=1
    fi
    if ! compile "$prg" "$OUT/$name.s32x" >"$OUT/$name.cc.log" 2>&1; then
        printf '%-28s NOCOMPILE %s\n' "$name" "$(grep -m1 -E 'unlowered|error|not supported|SUSPEND' "$OUT/$name.cc.log" | sed 's/.*: //' | cut -c1-60)"
        nocomp=$((nocomp+1)); continue
    fi
    # without its two copies a program would run wherever this was started
    # (busybox mktemp refused a short template once, and the corpus wrote
    # its tables into the caller's directory)
    wi=$(mktemp -d "$OUT/i.XXXXXX") && wc=$(mktemp -d "$OUT/c.XXXXXX") &&
        [ -d "$wi" ] && [ -d "$wc" ] || { echo "run-oracle: mktemp failed"; exit 2; }
    cp -R "$dir/." "$wi/"; cp -R "$dir/." "$wc/"
    setup=$(setup_for "$dir" "$name")
    if [ -n "$setup" ]; then
        for w in "$wi" "$wc"; do
            (cd "$w" && { printf '%s\n' "$setup"; printf 'QUIT\n'; } |
                timeout 60 "$EMU" -q --deny term "$DBASE" >/dev/null 2>&1)
        done
    fi
    # DO by the file's own name: the interpreter then names the program as
    # the compiled one does, on any file system (and `DO case` would be
    # the start of a DO CASE)
    (cd "$wi" && { printf 'SET TALK OFF\nDO %s\n' "$(basename "$prg")"
                   [ "$haskeys" = 1 ] && cat "$OUT/$name.keysin"
                   printf 'QUIT\n'; } |
        timeout 60 "$EMU" -q --deny term "$DBASE" 2>&1 | strip_interp) > "$OUT/$name.interp"
    if [ "$haskeys" = 1 ]; then
        (cd "$wc" && timeout 60 "$EMU" -q --deny term "$OUT/$name.s32x" < "$OUT/$name.keysin" 2>&1) > "$OUT/$name.clipraw"
    else
        (cd "$wc" && timeout 60 "$EMU" -q --deny term "$OUT/$name.s32x" </dev/null 2>&1) > "$OUT/$name.clipraw"
    fi
    rstrip_nl "$OUT/$name.clipraw" > "$OUT/$name.clip"
    rm -rf "$wi" "$wc"
    if cmp -s "$OUT/$name.interp" "$OUT/$name.clip"; then
        printf '%-28s AGREE\n' "$name"; agree=$((agree+1))
    elif upto_error "$OUT/$name.interp" "$OUT/$name.clip"; then
        printf '%-28s AGREE (to its error)\n' "$name"; agree=$((agree+1))
    elif stops_as_listed "$name" "$OUT/$name.interp" "$OUT/$name.clip"; then
        printf '%-28s AGREE (stops, as Clipper does)\n' "$name"; agree=$((agree+1))
    else
        printf '%-28s DIFFER (diff %s/%s.{interp,clip})\n' "$name" "$OUT" "$name"; differ=$((differ+1))
    fi
done
echo "xbc oracle: $agree agree, $differ differ, $nocomp do not compile, $skip skipped (of ${#CORPUS[@]})"
[ "$differ" -eq 0 ]
