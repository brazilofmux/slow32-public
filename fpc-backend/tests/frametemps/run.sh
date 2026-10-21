#!/bin/bash
# Frame temporaries below the register save area (GitHub issue 114), under
# -O2 -CfSLOW32 against the FPU RTL: a callee that keeps more than thirteen
# callee-saved registers live across a call used to save them where its own
# locals lived, and the caller's values came back clobbered.  Self-checking
# against expected.txt, which a host Free Pascal build of the same program
# prints.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
BACKEND="$(cd "$HERE/../.." && pwd)"
ROOT="$(cd "$BACKEND/.." && pwd)"
PPCS32="${PPCS32:-$HOME/fpc/compiler/ppcs32}"
RTL_DIR="${RTL_DIR:-$BACKEND/runtime/fpu}"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
RT="$ROOT/runtime"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
W="$(mktemp -d "$HOME/.frametemps.XXXXXX")"
trap 'rm -rf "$W"' EXIT
fail() { echo "frametemps: FAIL ($1)"; exit 1; }
"$PPCS32" -Tembedded -s -n -O2 -CfSLOW32 -Fu"$RTL_DIR" -FE"$W" "$HERE/frametemps.pas" > "$W/pas.log" 2>&1 || { cat "$W/pas.log"; fail "ppcs32"; }
"$AS" "$W/frametemps.s" "$W/frametemps.s32o" > /dev/null 2>&1 || fail "assemble"
"$AS" "$RTL_DIR/system.s" "$W/system.s32o" > /dev/null 2>&1 || fail "assemble system"
"$AS" "$BACKEND/runtime/fpc_startup.s" "$W/fpc_startup.s32o" > /dev/null 2>&1 || fail "assemble startup"
"$LD" --mmio 64K -o "$W/frametemps.s32x" "$RT/crt0.s32o" "$W/fpc_startup.s32o" "$W/system.s32o" \
    "$W/frametemps.s32o" "$RT/libc_mmio.s32a" "$RT/libs32.s32a" > "$W/ld.log" 2>&1 || { cat "$W/ld.log"; fail "link"; }
"$EMU" "$W/frametemps.s32x" 2>/dev/null \
    | grep -vE "^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code|Instructions|Simulated|Wall time|Performance|Cycles)|^$" > "$W/out.txt"
if diff "$HERE/expected.txt" "$W/out.txt" > "$W/diff.txt"; then
    echo "frametemps: PASS"
else
    cat "$W/diff.txt"
    fail "output"
fi
