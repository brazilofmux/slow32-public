#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
WORKDIR="$(mktemp -d /tmp/ld-x64-archive-machine.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

cd "$ROOT"

gcc -O2 -Wall -Wextra -o "$WORKDIR/ld-x64" selfhost/stage07-cross/ld-x64.c

cat >"$WORKDIR/start.s" <<'EOF'
.globl _start
_start:
    call foo
    mov $60, %eax
    xor %edi, %edi
    syscall
EOF

cat >"$WORKDIR/foo.c" <<'EOF'
int foo(void) {
    return 7;
}
EOF

gcc -c -O2 -ffreestanding -fno-pie -no-pie -o "$WORKDIR/start.o" "$WORKDIR/start.s"
gcc -c -O2 -ffreestanding -fno-pie -no-pie -o "$WORKDIR/foo-good.o" "$WORKDIR/foo.c"
cp "$WORKDIR/foo-good.o" "$WORKDIR/foo-bad.o"
printf '\003\000' | dd of="$WORKDIR/foo-bad.o" bs=1 seek=18 conv=notrunc status=none
ar rcs "$WORKDIR/libbad.a" "$WORKDIR/foo-bad.o"

set +e
"$WORKDIR/ld-x64" -o "$WORKDIR/a.out" "$WORKDIR/start.o" "$WORKDIR/libbad.a" \
    >"$WORKDIR/stdout.txt" 2>"$WORKDIR/stderr.txt"
rc=$?
set -e

if [ "$rc" -eq 0 ]; then
    echo "ld-x64 unexpectedly accepted a wrong-architecture archive member" >&2
    exit 1
fi

if ! grep -q "archive member is not x86-64" "$WORKDIR/stderr.txt"; then
    echo "ld-x64 did not report the expected archive-member architecture error" >&2
    cat "$WORKDIR/stderr.txt" >&2
    exit 1
fi
