#!/usr/bin/env bash
set -euo pipefail

# Generate Ragel -G2 lexer.
# No post-processing needed -- stage11 s32-cc handles:
#   - #line directives (pp_dir skips unknown directives)
#   - Prefix ++/-- (p_unary TK_INC/TK_DEC)
#   - Duff's device pattern (nested switch fix)
#   - const qualifier (p_btype skips it)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

ragel -G2 -o "$SCRIPT_DIR/c_lexer_gen.c" "$SCRIPT_DIR/c_lexer.rl"

echo "Generated: $(wc -l < "$SCRIPT_DIR/c_lexer_gen.c") lines, $(wc -c < "$SCRIPT_DIR/c_lexer_gen.c") bytes"
