#!/bin/bash
# Run nano buffer unit tests
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NANO_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$NANO_DIR")"

# Always build: nano.s32x is checked in, so existence does not prove that it
# matches the source under test.
echo "Building nano..."
cd "$NANO_DIR" && bash build.sh

echo "=== Running nano buffer tests ==="
if "$ROOT_DIR/tools/emulator/slow32-fast" "$NANO_DIR/nano.s32x" --test 2>&1; then
    echo "=== All tests passed ==="
else
    rc=$?
    echo "=== TESTS FAILED (exit code $rc) ==="
    exit $rc
fi
