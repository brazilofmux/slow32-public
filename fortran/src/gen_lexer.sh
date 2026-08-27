#!/usr/bin/env bash
set -euo pipefail
# Generate the Ragel -G2 Fortran token scanner.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ragel -G2 -o "$SCRIPT_DIR/f77_lexer_gen.c" "$SCRIPT_DIR/f77_lexer.rl"
echo "Generated: $(wc -l < "$SCRIPT_DIR/f77_lexer_gen.c") lines"
