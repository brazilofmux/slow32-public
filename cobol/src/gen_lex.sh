#!/usr/bin/env bash
# Regenerate the Ragel -G2 token scanner.  The output is checked in so
# the build needs no ragel; run this after editing lex.rl.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
# from the source directory with relative paths: the #line directives
# then name the .rl and the .c, not this checkout's absolute path, so
# regenerating elsewhere leaves the file unchanged
(cd "$HERE" && ragel -G2 -o lex_scan.c lex.rl)
echo "Generated: $(wc -l < "$HERE/lex_scan.c") lines"
