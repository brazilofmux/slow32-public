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
set -u

CLIP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(dirname "$CLIP_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
DBASE="$ROOT/xbase/xbase.s32x"
OUT="$CLIP_DIR/out/oracle"
mkdir -p "$OUT"

(cd "$CLIP_DIR" && bash build.sh >/dev/null) || { echo "s32-xbc build failed"; exit 2; }

if [ $# -gt 0 ]; then
    CORPUS=("$@")
else
    CORPUS=("$CLIP_DIR"/tests/*.prg "$ROOT"/xbase/tests/*.PRG)
fi

# Banner (3 lines), then the prompts ". " for SET TALK OFF and DO, the
# program's output, and the prompt for QUIT: strip exactly those.  ? starts
# its line, so the program may end mid-line, and the QUIT prompt then
# starts one of its own: trailing newlines are not compared (either side).
strip_interp() {
    python3 -c '
import sys
s = sys.stdin.buffer.read()
lines = s.split(b"\n", 3)
s = lines[3] if len(lines) > 3 else b""
if s.startswith(b". . "): s = s[4:]
if s.endswith(b". "): s = s[:-2]
sys.stdout.buffer.write(s.rstrip(b"\n"))'
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

agree=0; differ=0; nocomp=0; skip=0
for prg in "${CORPUS[@]}"; do
    name=$(basename "$prg"); name="${name%.*}"
    dir=$(cd "$(dirname "$prg")" && pwd)
    first=$(grep -viE '^[[:space:]]*(\*|&&|NOTE\b|$)' "$prg" | head -1)
    if echo "$first" | grep -qiE '^[[:space:]]*(FUNCTION|PROCEDURE)\b'; then
        printf '%-28s SKIP (a library: reached through its caller)\n' "$name"; skip=$((skip+1)); continue
    fi
    if grep -qiE '^[[:space:]]*(ACCEPT|INPUT|WAIT|READ)\b|INKEY\(' "$prg"; then
        printf '%-28s SKIP (reads the keyboard)\n' "$name"; skip=$((skip+1)); continue
    fi
    if ! bash "$CLIP_DIR/compile-prg.sh" "$prg" "$OUT/$name.s32x" >"$OUT/$name.cc.log" 2>&1; then
        printf '%-28s NOCOMPILE %s\n' "$name" "$(grep -m1 -E 'unlowered|error|not supported|SUSPEND' "$OUT/$name.cc.log" | sed 's/.*: //' | cut -c1-60)"
        nocomp=$((nocomp+1)); continue
    fi
    wi=$(mktemp -d "$OUT/i.XXXX"); wc=$(mktemp -d "$OUT/c.XXXX")
    cp -R "$dir/." "$wi/"; cp -R "$dir/." "$wc/"
    setup=$(setup_for "$dir" "$name")
    if [ -n "$setup" ]; then
        for w in "$wi" "$wc"; do
            (cd "$w" && { printf '%s\n' "$setup"; printf 'QUIT\n'; } |
                timeout 60 "$EMU" -q --deny term "$DBASE" >/dev/null 2>&1)
        done
    fi
    (cd "$wi" && printf 'SET TALK OFF\nDO %s\nQUIT\n' "$(basename "$prg")" |
        timeout 60 "$EMU" -q --deny term "$DBASE" 2>&1 | strip_interp) > "$OUT/$name.interp"
    (cd "$wc" && timeout 60 "$EMU" -q --deny term "$OUT/$name.s32x" </dev/null 2>&1) |
        python3 -c 'import sys; sys.stdout.buffer.write(sys.stdin.buffer.read().rstrip(b"\n"))' > "$OUT/$name.clip"
    rm -rf "$wi" "$wc"
    if cmp -s "$OUT/$name.interp" "$OUT/$name.clip"; then
        printf '%-28s AGREE\n' "$name"; agree=$((agree+1))
    else
        printf '%-28s DIFFER (diff %s/%s.{interp,clip})\n' "$name" "$OUT" "$name"; differ=$((differ+1))
    fi
done
echo "xbc oracle: $agree agree, $differ differ, $nocomp do not compile, $skip skipped (of ${#CORPUS[@]})"
[ "$differ" -eq 0 ]
