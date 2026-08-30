#!/bin/bash
# Build the compiled ship demo: ship-words + the interactive FLY main.
set -e
cd "$(dirname "$0")"
cat ship-words.fth fly-main.fth > ship.fth
bash ../compile.sh --hosted ship.fth ship.s32x
echo "run:   ../../tools/emulator/slow32-fast ship.s32x   (or slow32-dbt)"
echo "glass: ../../tools/s32-crt-mac"
