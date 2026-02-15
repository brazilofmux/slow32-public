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
ASM_FTH="${STAGE3_ASM:-$ROOT_DIR/selfhost/v2/stage01/asm.fth}"
AR_FTH="${STAGE3_AR:-$ROOT_DIR/selfhost/v2/stage02/ar.fth}"

usage() {
    cat <<USAGE
Usage: $0 [kernel|test3|archive]

Runs stage3 linker checks with path-override aware defaults.
Env overrides:
  STAGE3_EMU STAGE3_KERNEL STAGE3_PRELUDE STAGE3_LINK STAGE3_ASM STAGE3_AR
USAGE
}

TARGET="${1:-test3}"

for f in "$EMU" "$KERNEL" "$PRELUDE" "$LINK_FTH" "$ASM_FTH" "$AR_FTH"; do
    [[ -f "$f" ]] || { echo "Missing required file: $f" >&2; exit 1; }
done

WORKDIR="$(mktemp -d /tmp/stage3-regression.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

run_forth() {
    local script_file="$1"
    shift
    local cmd_text="$1"
    local log_file="$2"

    set +e
    cat "$PRELUDE" "$script_file" - <<FTH | timeout 120 "$EMU" "$KERNEL" >"$log_file" 2>&1
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
        OBJ="$WORKDIR/test3-forth.s32o"
        run_forth "$ASM_FTH" "S\" $ROOT_DIR/selfhost/v2/stage01/test3.s\" S\" $OBJ\" ASSEMBLE" "$WORKDIR/test3-asm.log"
        [[ -s "$OBJ" ]] || { echo "assembler produced no output" >&2; exit 1; }
        OUT="$WORKDIR/test3-forth-linked.s32x"
        CMD="LINK-INIT
S\" $OBJ\" LINK-OBJ
S\" $OUT\" LINK-EMIT"
        ;;
    archive)
        MINI_S="$WORKDIR/main_halt.s"
        MINI_O="$WORKDIR/main_halt.s32o"
        MINI_A="$WORKDIR/libmini.s32a"
        OUT="$WORKDIR/archive-linked.s32x"

        cat >"$MINI_S" <<'ASM'
.text
main:
    .global main
    halt
ASM

        run_forth "$ASM_FTH" "S\" $MINI_S\" S\" $MINI_O\" ASSEMBLE" "$WORKDIR/archive-asm.log"
        [[ -s "$MINI_O" ]] || { echo "assembler produced no output" >&2; exit 1; }

        run_forth "$AR_FTH" "S\" $MINI_A\" AR-C-BEGIN
S\" $MINI_O\" AR-ADD
AR-C-END" "$WORKDIR/archive-ar.log"
        [[ -s "$MINI_A" ]] || { echo "archiver produced no output" >&2; exit 1; }

        CMD="LINK-INIT
S\" $ROOT_DIR/runtime/crt0.s32o\" LINK-OBJ
S\" $MINI_A\" LINK-ARCHIVE
65536 LINK-MMIO
S\" $ROOT_DIR/runtime/libc_mmio.s32a\" LINK-ARCHIVE
S\" $ROOT_DIR/runtime/libs32.s32a\" LINK-ARCHIVE
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

run_forth "$LINK_FTH" "$CMD" "$WORKDIR/${TARGET}.log"
[[ -s "$OUT" ]] || { echo "linker produced no output" >&2; exit 1; }

echo "OK: stage3 $TARGET"
echo "Output: $OUT"
