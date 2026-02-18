#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SELFHOST_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$SELFHOST_DIR/.." && pwd)"

EMU="${STAGE01_EMU:-$SELFHOST_DIR/stage00/s32-emu}"
KERNEL="${STAGE01_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE01_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
ASM_FTH="${STAGE01_ASM:-$SCRIPT_DIR/asm.fth}"
TEST_DIR="${STAGE01_TEST_DIR:-$SCRIPT_DIR}"
TIMEOUT_SEC="${STAGE01_TIMEOUT_SEC:-120}"

usage() {
    cat <<USAGE
Usage: $0 [test1|test2|test3|test3b]

Runs stage01 assembler checks.
Env overrides:
  STAGE01_EMU STAGE01_KERNEL STAGE01_PRELUDE STAGE01_ASM STAGE01_TEST_DIR
  STAGE01_TIMEOUT_SEC
USAGE
}

TARGET="${1:-test1}"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$ASM_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage01-regression.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

run_forth() {
    local cmd_text="$1"
    local log_file="$2"

    set +e
    cat "$PRELUDE" "$ASM_FTH" - <<FTH | timeout "$TIMEOUT_SEC" "$EMU" "$KERNEL" >"$log_file" 2>&1
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
    test1|test2|test3|test3b)
        SRC="$TEST_DIR/${TARGET}.s"
        OUT="$WORKDIR/${TARGET}.s32o"
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

echo "OK: stage01 $TARGET"
echo "Output: $OUT"
