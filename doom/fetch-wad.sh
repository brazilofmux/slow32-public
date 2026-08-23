#!/bin/bash
# Fetch the Doom shareware WAD (freely distributable, ~4MB).
set -e
cd "$(dirname "$0")"
if [ -f doom1.wad ]; then
    echo "doom1.wad already here"
    exit 0
fi
for url in \
    "https://distro.ibiblio.org/slitaz/sources/packages/d/doom1.wad" \
    "https://www.quaddicted.com/files/idgames/idstuff/doom/doom19s.zip"; do
    echo "trying $url"
    if curl -fL --connect-timeout 15 -o doom1.wad.tmp "$url"; then
        case "$url" in
            *.wad) mv doom1.wad.tmp doom1.wad ;;
            *) echo "zip fallback needs manual extraction"; exit 1 ;;
        esac
        break
    fi
done
[ -f doom1.wad ] && echo "OK $(wc -c < doom1.wad) bytes" || exit 1
