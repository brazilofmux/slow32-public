#!/usr/bin/env bash
# gen_ref.sh — assemble tests/ref.s with gas, extract the .text bytes into ref.bin
set -euo pipefail
cd "$(dirname "$0")/.."

as -march=armv8-a -o tests/ref.o tests/ref.s

# Pull just the .text section bytes (no headers) into ref.bin
objcopy -O binary -j .text tests/ref.o tests/ref.bin
echo "wrote tests/ref.bin ($(stat -c%s tests/ref.bin) bytes)"
