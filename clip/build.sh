#!/bin/bash
# s32-clip, a host program: s32-clip.c with dbase's sources (main.c aside),
# whose parser it runs at compile time.  Rebuilt when any of them changes.
set -e
cd "$(dirname "$0")"
DB=../dbase/src
SRCS=$(ls $DB/*.c | grep -v '/main\.c$')
if [ -x s32-clip ] && [ -z "$(find s32-clip.c $SRCS $DB/*.h -newer s32-clip 2>/dev/null)" ]; then
    exit 0
fi
echo "=== Building s32-clip (host) ==="
# s32-clip.c held to -Wall -Wextra; dbase's sources built as they are
gcc -Wall -Wextra -O2 -I$DB -fsyntax-only s32-clip.c
gcc -O2 -I$DB -w -o s32-clip s32-clip.c $SRCS -lm
echo "OK  ./s32-clip"
