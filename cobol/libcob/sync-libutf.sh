#!/bin/bash
# Vendor the libutf units the COBOL runtime's locales need (docs/plans/
# locale.md): collation (UCA over DUCET, the 53 CLDR tailorings), NFC,
# and the tables they read.  libutf is MIT (its LICENSE comes along); the
# tables are Unicode data under the Unicode License V3, whose notice must
# go with every copy (LICENSE-UNICODE comes along too; NOTICE repeats it);
# cobol/ has to build in the toolchain image, where ~/utf is not, so the
# copies are committed and refreshed by hand with this script -- the
# casemap.h / s32utf_tables.h arrangement.
#   libcob/sync-libutf.sh [UTF_DIR]        (default ~/utf)
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
    echo "copied $(date +%F) by libcob/sync-libutf.sh: the units collation needs, nothing else"
} > "$D/SOURCE"
cat "$D/SOURCE"
du -sk "$D" | cut -f1 | sed 's/$/ KB/'
