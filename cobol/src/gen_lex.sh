#!/usr/bin/env bash
# Regenerate the Ragel -G2 token scanner.  The output is checked in so
# the build needs no ragel; run this after editing lex.rl.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ragel -G2 -o "$HERE/lex_scan.c" "$HERE/lex.rl"
echo "Generated: $(wc -l < "$HERE/lex_scan.c") lines"
