#!/bin/bash
# s32-bc, the BASIC compiler that decides up front (bc/, docs/plans/
# basic-compiler.md): the host program -- bc/bc.c with sbasic's front end
# (loader, lexer, parser, AST, checker, builtin names) -- and its runtime,
# bc/bc_rt.c compiled for SLOW-32 into out/bc_rt.s32o.  Rebuilt only when
# a source is newer.
set -e
cd "$(dirname "$0")"
SB=../sbasic/src
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
ASM="${S32_AS:-../tools/assembler/slow32asm}"
SRCS="$SB/program.c $SB/lexer.c $SB/parser.c $SB/ast.c $SB/value.c $SB/error.c $SB/strict.c $SB/builtin_names.c"
mkdir -p out
if [ ! -x s32-bc ] || [ -n "$(find bc/bc.c bc/bc_contract.h bc/hir $SRCS $SB/*.h $SB/builtins.def -newer s32-bc 2>/dev/null)" ]; then
    cc -O1 -g -I$SB -Ibc -w -o s32-bc bc/bc.c $SRCS -lm
    echo "OK  ./s32-bc"
fi
if [ ! -f out/bc_rt.s32o ] || [ -n "$(find bc/bc_rt.c $SB/*.h -newer out/bc_rt.s32o 2>/dev/null)" ]; then
    W="$(mktemp -d "${TMPDIR:-/tmp}/bcrt.XXXXXX")"
    trap 'rm -rf "${W:?}"' EXIT
    "$LLVM_BIN/clang" -target slow32-unknown-none -S -emit-llvm -nostdinc -fno-builtin \
        -I../runtime/include -I$SB -O2 bc/bc_rt.c -o "$W/bc_rt.ll"
    "$LLVM_BIN/llc" -mtriple=slow32-unknown-none "$W/bc_rt.ll" -o "$W/bc_rt.s"
    "$ASM" "$W/bc_rt.s" out/bc_rt.s32o >/dev/null
    echo "OK  out/bc_rt.s32o"
fi
