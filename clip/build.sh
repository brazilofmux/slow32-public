#!/bin/bash
# Build the host s32-clip compiler. Use ./compile-prg.sh to compile a .prg.
set -e
cd "$(dirname "$0")"
echo "=== Building s32-clip (host) ==="
gcc -Wall -Wextra -O2 -o s32-clip s32-clip.c
echo "OK  ./s32-clip"
echo "Compile a program with: ./compile-prg.sh tests/hello.prg"
