#!/bin/bash
# SLOW-32 Convenience Aliases
# Source this file in your shell session for quick access to Docker emulators
# Usage: source scripts/aliases.sh

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Create aliases for Docker-based emulator execution
alias s32run="$SCRIPT_DIR/run-in-docker.sh"
alias s32run-fast="$SCRIPT_DIR/run-in-docker.sh --fast"
alias s32run-qemu="$SCRIPT_DIR/run-in-docker.sh --qemu"
alias s32run-trace="$SCRIPT_DIR/run-in-docker.sh -t"
alias s32run-debug="$SCRIPT_DIR/run-in-docker.sh -t -r"

echo "SLOW-32 Docker aliases loaded:"
echo "  s32run         - Run with standard slow32 emulator"
echo "  s32run-fast    - Run with optimized slow32-fast emulator"
echo "  s32run-qemu    - Run with QEMU TCG emulator"
echo "  s32run-trace   - Run with instruction tracing"
echo "  s32run-debug   - Run with tracing + register display"
echo ""
echo "Examples:"
echo "  s32run program.s32x"
echo "  s32run-fast -c 1000 program.s32x"
echo "  s32run-qemu program.s32x"
