#!/bin/bash
# Host-side wrap check for GETSOCKNAME/GETPEERNAME (GitHub issue 110).
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/../../.." && pwd)"
cc -Wall -Wextra -O2 -o "$DIR/sockaddr_wrap" "$DIR/sockaddr_wrap.c"
"$DIR/sockaddr_wrap"
rm -f "$DIR/sockaddr_wrap"
# Emulator rebuild (no QEMU rebuild).
make -C "$ROOT/tools/emulator" -j2
echo "=== sockaddr wrap tests passed ==="
