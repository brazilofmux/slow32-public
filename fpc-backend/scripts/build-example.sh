#!/bin/bash
# Build and optionally run an FPC program for SLOW-32
#
# Usage: ./build-example.sh program.pas [--run]
#
# Prerequisites:
#   - ppcs32 cross-compiler built (see README.md)
#   - SLOW-32 toolchain built (make in project root)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FPC_DIR="$SCRIPT_DIR/.."
FPC_RUNTIME="$FPC_DIR/runtime"
PROJECT_DIR="$FPC_DIR/.."
TOOLCHAIN="$PROJECT_DIR/tools"
RUNTIME="$PROJECT_DIR/runtime"

# FPC cross-compiler location
PPCS32="${PPCS32:-$HOME/fpc/compiler/ppcs32}"

# Parse arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 program.pas [--run] [--fpu]"
    echo ""
    echo "Options:"
    echo "  --run    Run the program on the emulator after building"
    echo "  --fpu    Compile with -CfSLOW32 (native f32 FPU) against the FPU RTL"
    echo ""
    echo "Environment variables:"
    echo "  PPCS32   - path to ppcs32 cross-compiler (default: ~/fpc/compiler/ppcs32)"
    echo "  RTL_DIR  - path to compiled RTL (default: fpc-backend/runtime, or"
    echo "             fpc-backend/runtime/fpu with --fpu)"
    exit 1
fi

SOURCE="$1"
shift
RUN=0
FPU=0
FPCFLAGS=""
for arg in "$@"; do
    case "$arg" in
        --run) RUN=1 ;;
        --fpu) FPU=1 ;;
        *) echo "Unknown option: $arg"; exit 1 ;;
    esac
done

# RTL: use in-tree copy by default, override with RTL_DIR.
# The FPU RTL is a separate build (units must match the -Cf mode).
if [ $FPU -eq 1 ]; then
    RTL_DIR="${RTL_DIR:-$FPC_RUNTIME/fpu}"
    FPCFLAGS="-CfSLOW32"
else
    RTL_DIR="${RTL_DIR:-$FPC_RUNTIME}"
fi

BASENAME="$(basename "$SOURCE" .pas)"
OUTDIR="/tmp/slow32out"
mkdir -p "$OUTDIR"

# Check prerequisites
if [ ! -x "$PPCS32" ]; then
    echo "Error: ppcs32 not found at $PPCS32"
    echo "Set PPCS32 environment variable or build FPC cross-compiler first"
    exit 1
fi

if [ ! -f "$RTL_DIR/system.ppu" ]; then
    echo "Error: system.ppu not found in $RTL_DIR"
    echo "Compile the system unit first (see README.md step 3)"
    exit 1
fi

if [ ! -f "$RTL_DIR/system.s" ]; then
    echo "Error: system.s not found in $RTL_DIR"
    echo "Compile the system unit first (see README.md step 3)"
    exit 1
fi

# Step 1: Compile Pascal to assembly
echo "=== Compiling $SOURCE ==="
"$PPCS32" -Tembedded -s -n $FPCFLAGS \
    -Fu"$RTL_DIR" \
    -FE"$OUTDIR" \
    "$SOURCE"
echo "  -> $OUTDIR/$BASENAME.s"

# Step 2: Assemble all components
echo "=== Assembling ==="

# Assemble system unit (cached in $OUTDIR between runs; the FPU RTL is
# a distinct build, so it gets its own cached object)
if [ $FPU -eq 1 ]; then
    SYSTEM_OBJ="$OUTDIR/system-fpu.s32o"
else
    SYSTEM_OBJ="$OUTDIR/system.s32o"
fi
if [ ! -f "$SYSTEM_OBJ" ] || [ "$RTL_DIR/system.s" -nt "$SYSTEM_OBJ" ]; then
    "$TOOLCHAIN/assembler/slow32asm" "$RTL_DIR/system.s" "$SYSTEM_OBJ"
    echo "  -> system.s32o (assembled)"
else
    echo "  -> system.s32o (cached)"
fi

# Assemble FPC startup shim
STARTUP_OBJ="$OUTDIR/fpc_startup.s32o"
STARTUP_SRC="$FPC_RUNTIME/fpc_startup.s"
if [ ! -f "$STARTUP_OBJ" ] || [ "$STARTUP_SRC" -nt "$STARTUP_OBJ" ]; then
    "$TOOLCHAIN/assembler/slow32asm" "$STARTUP_SRC" "$STARTUP_OBJ"
    echo "  -> fpc_startup.s32o"
fi

# Assemble program
"$TOOLCHAIN/assembler/slow32asm" "$OUTDIR/$BASENAME.s" "$OUTDIR/$BASENAME.s32o"
echo "  -> $BASENAME.s32o"

# Step 3: Link
echo "=== Linking ==="
"$TOOLCHAIN/linker/s32-ld" --mmio 64K \
    -o "$OUTDIR/$BASENAME.s32x" \
    "$RUNTIME/crt0.s32o" \
    "$STARTUP_OBJ" \
    "$SYSTEM_OBJ" \
    "$OUTDIR/$BASENAME.s32o" \
    "$RUNTIME/libc_mmio.s32a" \
    "$RUNTIME/libs32.s32a"
echo "  -> $OUTDIR/$BASENAME.s32x"

# Step 4: Run (if requested)
if [ $RUN -eq 1 ]; then
    echo ""
    echo "=== Running ==="
    "$TOOLCHAIN/emulator/slow32-fast" "$OUTDIR/$BASENAME.s32x"
fi
