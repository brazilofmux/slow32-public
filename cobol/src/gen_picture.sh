#!/usr/bin/env bash
# Regenerate the Ragel -G2 PICTURE scanner.  The output is checked in so
# the build needs no ragel; run this after editing picture.rl.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ragel -G2 -o "$HERE/picture_scan.c" "$HERE/picture.rl"
echo "Generated: $(wc -l < "$HERE/picture_scan.c") lines"
