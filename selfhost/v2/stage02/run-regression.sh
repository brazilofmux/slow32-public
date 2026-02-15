#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="${STAGE2_AR_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
if git -C "$SCRIPT_DIR" rev-parse --show-toplevel >/dev/null 2>&1; then
    ROOT_DIR="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
fi

EMU="${STAGE2_AR_EMU:-$ROOT_DIR/tools/emulator/slow32}"
KERNEL="${STAGE2_AR_KERNEL:-$ROOT_DIR/forth/kernel.s32x}"
PRELUDE="${STAGE2_AR_PRELUDE:-$ROOT_DIR/forth/prelude.fth}"
ASM_FTH="${STAGE2_AR_ASM:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
AR_FTH="${STAGE2_AR_AR:-$SCRIPT_DIR/ar.fth}"
ASM_SRC="${STAGE2_AR_SRC:-$ROOT_DIR/selfhost/v2/stage01/test3.s}"

if [[ "$EMU" != /* ]]; then
    EMU="$ROOT_DIR/$EMU"
fi

usage() {
    cat <<USAGE
Usage: $0 [test3]

Runs stage02 archiver checks:
  1) Assemble stage01 test3.s -> test3.s32o
  2) Archive test3.s32o -> test3.s32a (AR-C-BEGIN/AR-ADD/AR-C-END)
  3) List archive and verify member name
  4) Extract member (AR-X1) and byte-compare with original

Env overrides:
  STAGE2_AR_EMU STAGE2_AR_KERNEL STAGE2_AR_PRELUDE
  STAGE2_AR_ASM STAGE2_AR_AR STAGE2_AR_SRC
USAGE
}

TARGET="${1:-test3}"
case "$TARGET" in
    test3) ;;
    -h|--help) usage; exit 0 ;;
    *)
        echo "Unknown target: $TARGET" >&2
        usage
        exit 2
        ;;
esac

for f in "$EMU" "$KERNEL" "$PRELUDE" "$ASM_FTH" "$AR_FTH" "$ASM_SRC"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage02-archiver.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

run_forth() {
    local script_a="$1"
    local script_b="$2"
    local cmd_text="$3"
    local log_file="$4"

    set +e
    cat "$PRELUDE" "$script_a" "$script_b" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$log_file" 2>&1
$cmd_text
FTH
    local rc=$?
    set -e
    if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
        echo "forth pipeline failed (rc=$rc)" >&2
        tail -n 40 "$log_file" >&2
        return 1
    fi
}

OBJ="$WORKDIR/test3.s32o"
ARC="$WORKDIR/test3.s32a"
XDIR="$WORKDIR/extract"
mkdir -p "$XDIR"

run_forth "$ASM_FTH" /dev/null "S\" $ASM_SRC\" S\" $OBJ\" ASSEMBLE
BYE" "$WORKDIR/as.log"
[[ -s "$OBJ" ]] || { echo "assembler produced no output" >&2; exit 1; }

run_forth "$AR_FTH" /dev/null "S\" $ARC\" AR-C-BEGIN
S\" $OBJ\" AR-ADD
AR-C-END
BYE" "$WORKDIR/ar-create.log"
[[ -s "$ARC" ]] || { echo "archiver produced no output" >&2; exit 1; }

run_forth "$AR_FTH" /dev/null "S\" $ARC\" AR-T
BYE" "$WORKDIR/ar-list.log"
grep -q "test3.s32o" "$WORKDIR/ar-list.log" || {
    echo "archive listing did not contain expected member" >&2
    tail -n 40 "$WORKDIR/ar-list.log" >&2
    exit 1
}

set +e
(
    cd "$XDIR"
    cat "$PRELUDE" "$AR_FTH" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$WORKDIR/ar-extract.log" 2>&1
S" $ARC" S" test3.s32o" AR-X1
BYE
FTH
)
rc=$?
set -e
if [[ "$rc" -ne 0 && "$rc" -ne 96 ]]; then
    echo "forth extraction failed (rc=$rc)" >&2
    tail -n 40 "$WORKDIR/ar-extract.log" >&2
    exit 1
fi
[[ -s "$XDIR/test3.s32o" ]] || { echo "extracted member missing" >&2; exit 1; }
cmp -s "$OBJ" "$XDIR/test3.s32o" || {
    echo "extracted object differs from original" >&2
    exit 1
}

echo "OK: stage02 $TARGET"
echo "Archive: $ARC"
