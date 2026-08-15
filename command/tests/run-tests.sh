#!/bin/bash
# Drive COMMAND.COM with a scripted session in a throwaway directory.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CMD_DIR="$(dirname "$SCRIPT_DIR")"
ROOT="$(dirname "$CMD_DIR")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"

echo "Building COMMAND.COM..."
(cd "$CMD_DIR" && bash build.sh)

if [ ! -x "$EMU" ]; then
    echo "emulator not found: $EMU"
    exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/s32-command.XXXXXX")"
cleanup() { rm -rf "$work"; }
trap cleanup EXIT

printf 'hello\n' > "$work/hello.txt"
cp "$SCRIPT_DIR/basic.cmd" "$work/basic.cmd"

echo "=== Running basic.cmd ==="
# The emulator prints a HALT footer on stdout; keep it out of the checks.
(cd "$work" && "$EMU" "$CMD_DIR/command.s32x") < "$work/basic.cmd" \
    > "$work/out.txt" 2>"$work/err.txt" || true

fail=0
check() {
    local pat="$1"
    if grep -q "$pat" "$work/out.txt"; then
        echo "  OK  $pat"
    else
        echo "  FAIL $pat"
        fail=1
    fi
}

check 'SLOW-32 Command'
check 'hello-from-command'
check 'hello'
check '1 file(s) copied'
check 'File not found'
check 'hello.txt'
check 'Bad command or file name'

if [ "$fail" -ne 0 ]; then
    echo "=== output ---"
    cat "$work/out.txt"
    echo "=== stderr ---"
    cat "$work/err.txt"
    echo "=== TESTS FAILED ==="
    exit 1
fi

echo "=== All tests passed ==="
