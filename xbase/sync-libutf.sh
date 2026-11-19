#!/bin/bash
# Vendor the libutf tables xbase's codepages need (src/codepage.c,
# docs/plans/xbase-utf8.md): the UTF-8 to CP437 and to ISO-8859-1
# approximation DFAs -- a character a table's codepage lacks is written as
# the nearest it has (ruling 2: a curly quote as a straight one).  Copied,
# not linked: xbase is built where ~/utf is not (the images, stage08).
# libutf is MIT (its LICENSE comes along); the tables are Unicode data
# under the Unicode License V3 (LICENSE-UNICODE comes along too).
#   xbase/sync-libutf.sh [UTF_DIR]        (default ~/utf)
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
UTF="${1:-$HOME/utf}"
SRC="$UTF/tables/charset_tables.c"
[ -f "$SRC" ] || { echo "sync-libutf: no libutf at $UTF" >&2; exit 1; }
D="$HERE/src/utf"
mkdir -p "$D"
cp "$UTF/LICENSE" "$D/LICENSE"
cp "$UTF/LICENSE-UNICODE" "$D/LICENSE-UNICODE"
{
    echo "/* libutf's UTF-8 to CP437 and to ISO-8859-1 approximation DFAs, extracted"
    echo " * from tables/charset_tables.c by xbase/sync-libutf.sh -- do not edit."
    echo " * libutf $(git -C "$UTF" log -1 --format='%h (%cs)' 2>/dev/null || echo '?'); MIT and Unicode License V3"
    echo " * (LICENSE, LICENSE-UNICODE beside this file). */"
    echo '#include "charset_approx.h"'
    awk '/^const unsigned (char|short) +tr_(cp437|latin1)_/ {p=1} p {print} p && /^};/ {p=0; print ""}' "$SRC"
} > "$D/charset_approx.c"
{
    echo "/* The DFAs in charset_approx.c (xbase/sync-libutf.sh): run over a"
    echo " * character's UTF-8 bytes from the start state, the state reached at or"
    echo " * past ACCEPTING_STATES_START, less that, is the byte in the codepage. */"
    echo "#ifndef XBASE_CHARSET_APPROX_H"
    echo "#define XBASE_CHARSET_APPROX_H"
    grep -E '^#define TR_(CP437|LATIN1)_' "$UTF/include/utf/utf_tables.h"
    grep -E '^extern UTF_API const unsigned (char|short) +tr_(cp437|latin1)_' "$UTF/include/utf/utf_tables.h" | sed 's/UTF_API //'
    echo "#endif"
} > "$D/charset_approx.h"
echo "sync-libutf: $D/charset_approx.c ($(wc -l < "$D/charset_approx.c") lines)"
