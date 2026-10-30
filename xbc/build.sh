#!/bin/bash
# s32-xbc, a host program: s32-xbc.c with xbase's sources (main.c aside),
# whose parser it runs at compile time.  Rebuilt when any of them changes.
set -e
cd "$(dirname "$0")"
DB=../xbase/src
SRCS=$(ls $DB/*.c | grep -v '/main\.c$')
if [ -x s32-xbc ] && [ -z "$(find s32-xbc.c $SRCS $DB/*.h -newer s32-xbc 2>/dev/null)" ]; then
    exit 0
fi
echo "=== Building s32-xbc (host) ==="
# s32-xbc.c held to -Wall -Wextra; xbase's sources built as they are
gcc -Wall -Wextra -O2 -I$DB -fsyntax-only s32-xbc.c
gcc -O2 -I$DB -w -o s32-xbc s32-xbc.c $SRCS -lm
echo "OK  ./s32-xbc"
