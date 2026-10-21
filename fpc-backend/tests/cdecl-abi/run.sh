#!/bin/bash
# cdecl ABI test (GitHub issue 98): a Pascal program and a C file, each
# compiled by its own compiler, calling each other with int64, double,
# single and varargs parameters in the shapes that tell the conventions
# apart: a double after an int (aligned pair, hole), an int64 after that
# hole (its halves split around the pair), a double or int64 at r10
# (stack, or straddling), fixed and variadic arguments of a varargs
# function.  The C side prints everything; the output must match
# expected.txt byte for byte.  Needs ppcs32, the compiled RTL and clang.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
BACKEND="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$BACKEND/.." && pwd)"
PPCS32="${PPCS32:-$HOME/fpc/compiler/ppcs32}"
RTL_DIR="${RTL_DIR:-$BACKEND/runtime}"
LLVM_BIN="${LLVM_BIN:-$HOME/llvm-project/build/bin}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"

W="$(mktemp -d "$HOME/.cdecl-abi.XXXXXX")"
trap 'rm -rf "$W"' EXIT

fail() { echo "cdecl-abi: FAIL ($1)"; exit 1; }

"$PPCS32" -Tembedded -s -n -Fu"$RTL_DIR" -FE"$W" "$HERE/abi.pas" > "$W/pas.log" 2>&1 || { cat "$W/pas.log"; fail "ppcs32"; }
"$LLVM_BIN/clang" -target slow32-unknown-none -S -O2 -I"$RT/include" "$HERE/cside.c" -o "$W/cside.s" 2> "$W/cc.log" || { cat "$W/cc.log"; fail "clang"; }
for f in abi cside; do
    "$AS" "$W/$f.s" "$W/$f.s32o" > /dev/null 2>&1 || fail "assemble $f"
done
"$AS" "$RTL_DIR/system.s" "$W/system.s32o" > /dev/null 2>&1 || fail "assemble system"
"$AS" "$BACKEND/runtime/fpc_startup.s" "$W/fpc_startup.s32o" > /dev/null 2>&1 || fail "assemble startup"
"$LD" --mmio 64K -o "$W/abi.s32x" "$RT/crt0.s32o" "$W/fpc_startup.s32o" "$W/system.s32o" \
    "$W/abi.s32o" "$W/cside.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$W/ld.log" 2>&1 || { cat "$W/ld.log"; fail "link"; }
"$EMU" "$W/abi.s32x" 2>/dev/null | grep -E "^(c_|p_)" > "$W/out.txt"
if diff "$HERE/expected.txt" "$W/out.txt" > "$W/diff.txt"; then
    echo "cdecl-abi: PASS"
else
    cat "$W/diff.txt"
    fail "output"
fi
