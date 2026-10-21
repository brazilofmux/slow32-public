#!/bin/bash
# 64-bit shifts by a variable count, modulo 64 as Free Pascal defines them
# (GitHub issue 98).  Self-checking against expected.txt, which a host
# Free Pascal build of the same program prints.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
BACKEND="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$BACKEND/.." && pwd)"
PPCS32="${PPCS32:-$HOME/fpc/compiler/ppcs32}"
RTL_DIR="${RTL_DIR:-$BACKEND/runtime}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
W="$(mktemp -d "$HOME/.shift64.XXXXXX")"
trap 'rm -rf "$W"' EXIT
fail() { echo "shift64: FAIL ($1)"; exit 1; }
"$PPCS32" -Tembedded -s -n -Fu"$RTL_DIR" -FE"$W" "$HERE/shift64.pas" > "$W/pas.log" 2>&1 || { cat "$W/pas.log"; fail "ppcs32"; }
"$AS" "$W/shift64.s" "$W/shift64.s32o" > /dev/null 2>&1 || fail "assemble"
"$AS" "$RTL_DIR/system.s" "$W/system.s32o" > /dev/null 2>&1 || fail "assemble system"
"$AS" "$BACKEND/runtime/fpc_startup.s" "$W/fpc_startup.s32o" > /dev/null 2>&1 || fail "assemble startup"
"$LD" --mmio 64K -o "$W/shift64.s32x" "$RT/crt0.s32o" "$W/fpc_startup.s32o" "$W/system.s32o" \
    "$W/shift64.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$W/ld.log" 2>&1 || { cat "$W/ld.log"; fail "link"; }
"$EMU" "$W/shift64.s32x" 2>/dev/null | grep -E "^i?sh[lr] " > "$W/out.txt"
if diff "$HERE/expected.txt" "$W/out.txt" > "$W/diff.txt"; then
    echo "shift64: PASS"
else
    cat "$W/diff.txt"
    fail "output"
fi
