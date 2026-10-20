#!/bin/bash
# Vendor the libutf units the named locales need (locales.c): collation
# (UCA over DUCET, the CLDR tailorings), NFC, and the tables they read --
# the same units cobol/libcob/sync-libutf.sh takes for the COBOL runtime.
# libutf is MIT (its LICENSE comes along); the tables are Unicode data
# under the Unicode License V3, whose notice goes with every copy
# (LICENSE-UNICODE comes along too).  The runtime is built in the
# toolchain image, where ~/utf is not, so the copies are committed and
# refreshed by hand with this script.
#   runtime/sync-libutf.sh [UTF_DIR]        (default ~/utf)
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
UTF="${1:-$HOME/utf}"
[ -f "$UTF/src/collate.c" ] || { echo "sync-libutf: no libutf at $UTF" >&2; exit 1; }
D="$HERE/utf"
mkdir -p "$D/src" "$D/tables" "$D/include/utf"
for f in src/collate.c src/nfc.c \
         tables/ducet_cetable.c tables/ducet_dfa_tables.c tables/nfc_tables.c tables/unicode_tables.c \
         include/utf/utf_types.h include/utf/collate.h include/utf/nfc.h include/utf/utf_tables.h \
         LICENSE LICENSE-UNICODE; do
    cp "$UTF/$f" "$D/$f"
done
{
    echo "libutf $(git -C "$UTF" describe --always --dirty 2>/dev/null) ($(git -C "$UTF" log -1 --format=%cs 2>/dev/null))"
    echo "copied $(date +%F) by runtime/sync-libutf.sh: the units collation needs, nothing else"
} > "$D/SOURCE"
cat "$D/SOURCE"
du -sk "$D" | cut -f1 | sed 's/$/ KB/'
