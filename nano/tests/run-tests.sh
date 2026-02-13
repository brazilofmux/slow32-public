#!/bin/bash
# Run nano buffer unit tests
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NANO_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$NANO_DIR")"

# Build if needed
if [ ! -f "$NANO_DIR/nano.s32x" ]; then
    echo "Building nano..."
    cd "$NANO_DIR" && bash build.sh
fi

echo "=== Running nano buffer tests ==="
"$ROOT_DIR/tools/emulator/slow32-fast" "$NANO_DIR/nano.s32x" --test 2>&1
rc=$?
if [ $rc -eq 0 ]; then
    echo "=== All tests passed ==="
else
    echo "=== TESTS FAILED (exit code $rc) ==="
fi
exit $rc
