#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
WORKDIR="$(mktemp -d /tmp/ld-x64-subsections.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT"

gcc -O2 -Wall -Wextra -o "$WORKDIR/ld-x64" selfhost/stage07-cross/ld-x64.c

gcc -c -O2 -ffreestanding -fno-pie -no-pie \
    -fno-asynchronous-unwind-tables -fno-unwind-tables -fno-stack-protector \
    -o "$WORKDIR/ld_subsections.o" selfhost/stage07-cross/tests/ld_subsections.c

"$WORKDIR/ld-x64" -o "$WORKDIR/ld_subsections" "$WORKDIR/ld_subsections.o"
"$WORKDIR/ld_subsections"
