#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE3_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE3_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${STAGE3_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE3_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
LINK_FTH="${STAGE3_LINK:-$SCRIPT_DIR/link.fth}"

usage() {
    cat <<USAGE
Usage: $0 [kernel|test3]

Runs stage3 linker checks with path-override aware defaults.
Env overrides:
  STAGE3_EMU STAGE3_KERNEL STAGE3_PRELUDE STAGE3_LINK
USAGE
}

TARGET="${1:-test3}"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$LINK_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage3-regression.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

run_forth() {
    local cmd_text="$1"
    local log_file="$2"

    set +e
    cat "$PRELUDE" "$LINK_FTH" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$log_file" 2>&1
$cmd_text
BYE
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 40 "$log_file" >&2
        return 1
    fi
}

case "$TARGET" in
    kernel)
        OUT="$WORKDIR/kernel-forth-linked.s32x"
        CMD="LINK-INIT
S\" $ROOT_DIR/runtime/crt0.s32o\" LINK-OBJ
S\" $ROOT_DIR/forth/kernel.s32o\" LINK-OBJ
65536 LINK-MMIO
S\" $ROOT_DIR/runtime/libc_mmio.s32a\" LINK-ARCHIVE
S\" $ROOT_DIR/runtime/libs32.s32a\" LINK-ARCHIVE
S\" $OUT\" LINK-EMIT"
        ;;
    test3)
        OUT="$WORKDIR/test3-forth-linked.s32x"
        CMD="LINK-INIT
S\" /tmp/test3-forth.s32o\" LINK-OBJ
S\" $OUT\" LINK-EMIT"
        ;;
    -h|--help)
        usage
        exit 0
        ;;
    *)
        echo "Unknown target: $TARGET" >&2
        usage
        exit 2
        ;;
esac

run_forth "$CMD" "$WORKDIR/${TARGET}.log"
[[ -s "$OUT" ]] || { echo "linker produced no output" >&2; exit 1; }

echo "OK: stage3 $TARGET"
echo "Output: $OUT"
