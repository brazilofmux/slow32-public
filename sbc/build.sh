#!/bin/bash
# s32-sbc, a host program: s32-sbc.c with sbasic's front end (the loader,
# lexer, parser, AST and lowering), which it runs at compile time.
# Rebuilt when any of them changes.
set -e
cd "$(dirname "$0")"
SB=../sbasic/src
SRCS="$SB/program.c $SB/lexer.c $SB/parser.c $SB/ast.c $SB/value.c $SB/error.c $SB/lower.c $SB/builtin_names.c"
if [ -x s32-sbc ] && [ -z "$(find s32-sbc.c $SRCS $SB/*.h -newer s32-sbc 2>/dev/null)" ]; then
    exit 0
fi
echo "=== Building s32-sbc (host) ==="
gcc -Wall -Wextra -O2 -I$SB -fsyntax-only s32-sbc.c
gcc -O2 -I$SB -w -o s32-sbc s32-sbc.c $SRCS -lm
echo "OK  ./s32-sbc"
