#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE2_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE2_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${STAGE2_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE2_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
ASM_FTH="${STAGE2_ASM:-$SCRIPT_DIR/asm.fth}"
TEST_DIR="${STAGE2_TEST_DIR:-$SCRIPT_DIR}"

usage() {
    cat <<USAGE
Usage: $0 [kernel|test1|test2|test3|test3b]

Runs stage2 assembler checks with path-override aware defaults.
Env overrides:
  STAGE2_EMU STAGE2_KERNEL STAGE2_PRELUDE STAGE2_ASM STAGE2_TEST_DIR
USAGE
}

TARGET="${1:-test1}"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$ASM_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage2-regression.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

run_forth() {
    local cmd_text="$1"
    local log_file="$2"

    set +e
    cat "$PRELUDE" "$ASM_FTH" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$log_file" 2>&1
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
        SRC="$ROOT_DIR/forth/kernel.s"
        OUT="$WORKDIR/kernel-forth.s32x"
        ;;
    test1|test2|test3|test3b)
        SRC="$TEST_DIR/${TARGET}.s"
        OUT="$WORKDIR/${TARGET}.s32x"
        [[ "$TARGET" == "test3" ]] && OUT="$WORKDIR/${TARGET}.s32o"
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

[[ -f "$SRC" ]] || { echo "Missing source: $SRC" >&2; exit 1; }
run_forth "S\" $SRC\" S\" $OUT\" ASSEMBLE" "$WORKDIR/${TARGET}.log"
[[ -s "$OUT" ]] || { echo "assembler produced no output" >&2; exit 1; }

echo "OK: stage2 $TARGET"
echo "Output: $OUT"
