#!/bin/bash
# Host gcc build of COMMAND.COM to check the launch token length (GitHub issue 111).
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CMD_DIR="$(dirname "$SCRIPT_DIR")"
work="$(mktemp -d "${TMPDIR:-/tmp}/s32-cmdlen.XXXXXX")"
cleanup() { rm -rf "$work"; }
trap cleanup EXIT

cat > "$work/s32_execv.c" <<'EOF'
#include <stdio.h>
int s32_execv(const char *path, char *const argv[]) {
    (void)argv;
    printf("EXEC %s\n", path);
    return 0;
}
EOF

gcc -Wall -Wextra -O2 -o "$work/command" \
    "$CMD_DIR/src/command.c" "$work/s32_execv.c"

# 40-character stem plus .s32x — used to truncate at 31.
name="abcdefghijklmnopqrstuvwxyz0123456789.s32x"
: > "$work/$name"

printf '%s\nEXIT\n' "$name" | (cd "$work" && ./command) > "$work/out.txt"

if grep -q "EXEC $name" "$work/out.txt"; then
    echo "  OK  long .s32x token"
else
    echo "  FAIL long .s32x token"
    cat "$work/out.txt"
    exit 1
fi
echo "=== host cmdlen tests passed ==="
