#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE4_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE4_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${STAGE4_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE4_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
CC_FTH="${STAGE4_CC:-$SCRIPT_DIR/cc.fth}"
SRC=""
OUT=""
KEEP_ARTIFACTS=0

usage() {
    cat <<USAGE
Usage: $0 --src <file.c> [--out <file.s>] [--emu <path>] [--keep-artifacts]

Compiles one source file with Stage4 compiler while forcing:
  DBG-LONG-BRANCH = 1

Prints the compiler's "[CC] Long branches lowered" summary.
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --src)
            shift
            [[ $# -gt 0 ]] || { echo "--src requires a path" >&2; exit 2; }
            SRC="$1"
            ;;
        --out)
            shift
            [[ $# -gt 0 ]] || { echo "--out requires a path" >&2; exit 2; }
            OUT="$1"
            ;;
        --emu)
            shift
            [[ $# -gt 0 ]] || { echo "--emu requires a path" >&2; exit 2; }
            EMU="$1"
            ;;
        --keep-artifacts)
            KEEP_ARTIFACTS=1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
    shift
done

[[ -n "$SRC" ]] || { echo "Missing required --src" >&2; usage; exit 2; }

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi
if [[ "$SRC" != /* ]]; then
    SRC="$ROOT_DIR/$SRC"
fi
if [[ -n "$OUT" && "$OUT" != /* ]]; then
    OUT="$ROOT_DIR/$OUT"
fi

for f in "$EMU" "$KERNEL" "$PRELUDE" "$CC_FTH" "$SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage4-ccdbg.XXXXXX)"
if [[ "$KEEP_ARTIFACTS" -eq 0 ]]; then
    trap 'rm -rf "$WORKDIR"' EXIT
fi

CC_DBG="$WORKDIR/cc.debug.fth"
cp "$CC_FTH" "$CC_DBG"

if ! sed -Ei 's/^[[:space:]]*[0-9]+([[:space:]]+CONSTANT[[:space:]]+DBG-LONG-BRANCH)([[:space:]]*.*)$/1      CONSTANT DBG-LONG-BRANCH\2/' "$CC_DBG"; then
    echo "Failed to enable DBG-LONG-BRANCH in temporary compiler copy" >&2
    exit 1
fi
if ! rg -q "^1[[:space:]]+CONSTANT[[:space:]]+DBG-LONG-BRANCH([[:space:]]|$)" "$CC_DBG"; then
    echo "Could not verify DBG-LONG-BRANCH override" >&2
    exit 1
fi

ASM_OUT="$WORKDIR/$(basename "${SRC%.c}").s"
if [[ -n "$OUT" ]]; then
    ASM_OUT="$OUT"
fi
LOG="$WORKDIR/cc.log"

set +e
cat "$PRELUDE" "$CC_DBG" - <<FTH | timeout 180 "$EMU" "$KERNEL" >"$LOG" 2>&1
S" $SRC" S" $ASM_OUT" COMPILE-FILE
BYE
FTH
rc=$?
set -e

if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
    echo "Compiler run failed (rc=$rc)" >&2
    tail -n 80 "$LOG" >&2
    exit 1
fi

if ! grep -q "\[CC\] Long branches lowered:" "$LOG"; then
    echo "Long-branch summary not found in compiler output" >&2
    tail -n 80 "$LOG" >&2
    exit 1
fi

grep "\[CC\] Long branches lowered:" "$LOG" | tail -n 1

if grep -q "Compilation successful" "$LOG" && [[ -s "$ASM_OUT" ]]; then
    echo "Output asm: $ASM_OUT"
else
    echo "Compilation did not complete successfully" >&2
    tail -n 80 "$LOG" >&2
    exit 1
fi

echo "Artifacts: $WORKDIR"
