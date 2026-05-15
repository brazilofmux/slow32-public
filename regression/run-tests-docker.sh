#!/bin/bash
# Run the SLOW-32 regression suite inside the slow32:toolchain Docker image.
#
# Useful on hosts that don't have a local LLVM checkout but do have the
# prebuilt slow32:toolchain image available (which ships clang/llc plus the
# musl-built slow32asm / s32-ld / s32-ar in /opt/slow32/bin).
#
# The container does NOT include a `slow32` emulator binary, so this wrapper
# builds one from tools/emulator/ on first run, into a host-side cache dir
# (regression/.docker-emu-bin/) so the host's own glibc-built
# tools/emulator/slow32 isn't disturbed.
#
# Usage:
#   ./regression/run-tests-docker.sh [test-name ...]
#
# Env vars:
#   SLOW32_TOOLCHAIN_IMAGE   override image tag (default: slow32:toolchain)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLOW32_BASE="$(cd "$SCRIPT_DIR/.." && pwd)"

IMAGE="${SLOW32_TOOLCHAIN_IMAGE:-slow32:toolchain}"

if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
    echo "Error: Docker image '$IMAGE' not found." >&2
    echo "Pull or build it first; see CLAUDE.md for the toolchain image." >&2
    exit 1
fi

# Cache the musl-built emulator in a host dir so we don't rebuild every run.
EMU_CACHE_DIR="$SCRIPT_DIR/.docker-emu-bin"
mkdir -p "$EMU_CACHE_DIR"

docker run --rm \
    -v "$SLOW32_BASE:/workspace" \
    -w /workspace \
    --user "$(id -u):$(id -g)" \
    -e HOME=/tmp \
    "$IMAGE" \
    bash -c '
        set -e

        EMU_BIN=regression/.docker-emu-bin/slow32
        if [ ! -x "$EMU_BIN" ] || \
           [ tools/emulator/slow32.c -nt "$EMU_BIN" ] || \
           [ tools/emulator/mmio_ring.c -nt "$EMU_BIN" ]; then
            echo "Building musl-linked slow32 emulator into $EMU_BIN ..."
            gcc -O2 -Wall \
                tools/emulator/slow32.c tools/emulator/mmio_ring.c \
                -Itools/emulator \
                -o "$EMU_BIN" -lm
        fi

        export LLVM_BIN=/opt/llvm/bin
        export ASSEMBLER=/opt/slow32/bin/slow32asm
        export LINKER=/opt/slow32/bin/s32-ld
        export EMULATOR="$PWD/$EMU_BIN"

        cd regression
        exec ./run-tests.sh "$@"
    ' -- "$@"
